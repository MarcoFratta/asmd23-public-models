package scala.u06.lab

import scala.annotation.targetName
import scala.reflect.ClassTag
import scala.u06.modelling.{PNetExtensions, SystemAnalysis}


object PetriNetFacade:
  import PNetExtensions.*
  import scala.u06.lab.PetriNet.*
  import Token.*
  import scala.u06.lab.PetriNet.Transition.*

  trait NoId
  private case class NoIdImpl() extends NoId:
    override def toString: String = "Token"


  object PNOperation:
    object Trn:
      def apply[I,T](name: String): Transition[I,T] = Transition[I,T](Map(), Map(), name)

    extension [P](values: P*)
      @targetName("Create a a sequence")
      def > :Seq[P] = List.from(values)

    def extract[T: ClassTag](b: Token[?]): Option[Token[T]] = b match
      case Value(t: T, n) => Some(Value(t, n))
      case _ => Option.empty[Token[T]]

    private def moveTokenWith[T: ClassTag](filter: Token[?] => Boolean)
                                          (vMap: T => ?)(nMap: String => String)
                                          (exp: T => Iterable[(?, String)]): Arc =
      (x: Cond) => extract[T](x).filter(filter) match
        case Some(v: Token[T]) => >() -> List[Token[?]](exp(v.getValue.get).map(x =>
          token(x._1, x._2)).toSeq.appended(token(vMap(v.getValue.get), nMap(v.getName))) *)

    def noIdToken: Token[NoId] = Token.Value(NoIdImpl(), "")
    def token[T](t: T, name: String = ""): Token[T] = Token.Value(t, name)
    def addToken[T](n: Int, v:Token[T]): Arc = (o:Token[?]) => box() -> box(n, v)
    def moveToken[T](v:Token[T]):Arc = moveTokenWith[NoId](_ == v)(identity)(identity)(_ => List())
    def moveNToken(n:Int):Arc = (0 until n - 1).foldLeft(moveToken(noIdToken))
      ((a,i) => a.andThen((l:Box, r:Box) => l.drop(1) -> (r ++ box(noIdToken))))

    extension [I](p: I)
      @targetName("Link a place to an ingoing arc")
      def <--(a: Arc): (I, Arc) = p -> a
      @targetName("Move n token from the place p")
      def <--(n: Int): Iterable[(I, Arc)] = 0 to n map (_ => p -> moveToken(noIdToken))
      @targetName("Consume a specific token from the place p")
      def <--[T](a: T): (I, Arc) = p -> moveToken(token(a))
      @targetName("Consume a token from the place p and than move it back to p")
      def <-->(a: Arc): (I, Arc) = p -> a.andThen((left, right) =>
        (left ++ right).toSeq.distinctBy(_.getName) -> right)

    def empty[T: ClassTag]: Arc = moveTokenWith[T](_ => true)(_ => Token.Empty[T]())(identity)(_ => List())

    @targetName("Moves a certain type of token after applying a function f to its value")
    def <>[T: ClassTag](f: T => ?): Arc = moveTokenWith[T](_ => true)(f)(identity)(_ => List())

    @targetName("Moves a certain type of token with name s after applying a function f")
    def <>[T: ClassTag](s: String)(f: T => ?): Arc =
      moveTokenWith[T](_.getName == s)(f)(identity)(_ => List())

    @targetName("Moves a certain type of token mapping its name to s")
    def :~[T: ClassTag](s: String): Arc = moveTokenWith[T](_ => true)(identity)(_ => s)(_ => List())

    @targetName("Create an arc that moves a certain type of token")
    def <>[T: ClassTag]: Arc = moveTokenWith[T](_ => true)(identity)(identity)(_ => List())

    @targetName("Create an arc that moves a certain type of token with the name s")
    def <>[T:ClassTag](s:String):Arc = moveTokenWith[T](_.getName == s)(identity)(identity)(_ => List())

    @targetName("Move a token of type T expanding it to a list of tokens using a function f")
    def ~+[T: ClassTag](f:T => Iterable[(?,String)]): Arc =
      moveTokenWith[T](_ => true)(identity)(identity)(f)

    @targetName("Move a token of type T and name s expanding it to a list of tokens using a function f")
    def +:[T: ClassTag](f: T => Iterable[(?, String)])(s:String): Arc =
      moveTokenWith[T](_.getName == s)(identity)(identity)(f)

    extension [I](m: Iterable[(I, Arc)])
      @targetName("Set the condition arcs for a transition")
      def ~~>(name: String): Transition[I,Token[?]] =
        Transition[I,Token[?]](m.map(x => (x._1, x._2)).groupMap(_._1)(_._2).map(x =>
          x._1 -> x._2.head), Map(), name)

    extension [I](t: Transition[I,Token[?]])
      @targetName("Set the effect arcs for a transition")
      def ~~>(m: Iterable[(Arc, I)]): Transition[I,Token[?]] =
        Transition(t.condition, m.map(x => x._1 -> x._2).toMap, t.name)
      @targetName("alias for withInhibitorArcs")
      def ^^^(i: Iterable[I]): Transition[I,Token[?]] = InhibitedTransition(t, i.toSet)


    extension [T](n: Int)
      @targetName("Create an arc that add n times a certain token")
      def <>(v:T): Arc = addToken(n, token(v))
    extension (a: Arc)
      @targetName("Link an outgoing arc to a place")
      def -->[I](p: I): (Arc, I) = a -> p
    extension [T](v:T)
      @targetName("Add a token to a place")
      def --> [I](p:I): (Arc, I) = addToken(1, token(v)) -> p


    extension [I,T](m:Seq[SystemAnalysis.Path[Marking[I,T]]])
      def prettyPrint:String = m.toList.map(x =>
      x.map(v => s"[${v.map(e => s"${e._1} -> ${e._2.mkString("{",",","}")}")
          .mkString(" , ")}]")
        .mkString(" <> ")
      ).mkString("\n")

  object BasicPN:
    import PetriNetFacade.PNOperation.*
    export PNOperation.>

    def apply[I](t: Set[Transition[I, Token[?]]]): PetriNetImpl[I] =
      PetriNetImpl[I](t)

    extension [I](cond: Iterable[I])
      @targetName("Create a transition with the given condition cond and effects eff")
      def ~~>(eff: Iterable[I]): Transition[I, Token[?]] =
        val condition = cond.groupMapReduce(identity)(x => 1)
          ((a, b) => a + b).map((i, n) => i -> moveNToken(n))
        val effect = eff.groupMapReduce(identity)(x => 1)
          ((a, b) => a + b).map((i, n) => addToken(n, noIdToken) -> i )
        Transition(condition, effect, "")

    def marking[I](m:(I,Int)*):Map[I,Box] =
      m.toList.map((i, n) => i -> List.fill(n)(noIdToken)).toMap
    def marking[I](m:Iterable[I]):Map[I,Box] =
      marking(m.toList.counted.toList*)

  object PN:
    export BasicPN.{apply => _, _}
    export PNOperation.^^^

    def apply[I](t: Set[Transition[I, Token[?]]]): PetriNetImpl[I] = BasicPN.apply(t)

  object CPN:
    export PNetExtensions.InhibitedTransition.*
    export PNOperation.*

    def apply[I](t: Transition[I, Token[?]]*): PetriNetImpl[I] = PetriNetImpl(t.toSet)
    //def marking[I,T](p: (I, Box)*): Map[I, Box] = p.toMap
    def anonMarking[I, T](p: (I, Iterable[T])*): Map[I, Box] = p.map((i, t) => i -> t.map(token(_))).toMap

    def marking[I, T](p: (I, Iterable[(T, String)])*): Map[I, Box] = p.map((i, t) =>
      i -> t.map(a => token(a._1,a._2))).toMap

  object CPPN:
    export CPN.{apply => _, _}
    export PNetExtensions.PriorityNet.*

    def apply[I](t: (Transition[I, Token[?]], Int)*): PetriNetImpl[I] =
      CPN(t.map((t,p) => t withPriority p)*)

    extension [I](t: Transition[I, Token[?]])
      @targetName("Set the priority of the transition")
      def #-#(n: Int): (Transition[I, Token[?]], Int) = t -> n

  object PPN:
    import CPPN.{apply as _, *}
    export PN.{apply => _ , _}
    export PNetExtensions.PriorityNet.*

    def apply[I](t: (Transition[I, Token[?]], Int)*): PetriNetImpl[I] =
      CPPN[I](t*)

    extension [I](t: Transition[I, Token[?]])
      @targetName("Set the priority of the transition")
      def #-#(n: Int): (Transition[I, Token[?]], Int) = CPPN.#-#(t)(n)





