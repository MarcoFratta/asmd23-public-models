package scala.u06.lab

import scala.annotation.targetName
import scala.reflect.{ClassTag, classTag}
import scala.u06.modelling.SystemAnalysis
import scala.u07.utils.MSet


object PetriNetFacade:
  import PNetExtensions.*
  import scala.u06.lab.PetriNetApi.*
  import Token.*
  import scala.u06.lab.PetriNetApi.Transition.*

  trait NoId
  private case class NoIdImpl() extends NoId:
    override def toString: String = "Token"


  object PNOperation:
    object Trn:
      def apply[I,T](name: String): Transition[I,T] = Transition.ofMap[I,T](Map[I, Iterable[Arc]](),
        Map[Arc, I](), name, true)

    extension [P](values: P*)
      @targetName("Create a a sequence")
      def > :Seq[P] = List.from(values)

    def extract[T:ClassTag](b: Token[?]): Option[Token[T]] = b match
      case Value(t: T, n) => Some(Value(t, n))
      case _ => Option.empty[Token[T]]


    private def inArcOf[T: ClassTag](filter: Token[?] => Boolean)
                                          (vMap: T => ?)(nMap: String => String)
                                          (exp: T => Iterable[(?, String)]): Arc =
      new PartialFunction[Box, Box]:
        override def isDefinedAt(x: Box): Boolean = x.flatMap(extract[T](_)).exists(filter)
        override def apply(x: Box): Box =
          x.map(extract[T](_)).filter(_.isDefined).map(_.get)
          .filter(filter).head match
          case Value(v: T, n) => List[Token[?]](exp(v).map(x =>
            token(x._1, x._2)).toSeq.appended(token(vMap(v), nMap(n))) *)



    //private def outArcOf[T: ClassTag](f: Box => Box): Arc = (x: Box) => f(x)

    def noValueToken: Token[NoId] = Token.Value(NoIdImpl(), "")
    def noIdToken[T](t:T):Token[T] = Token.Value(t, "")
    def token[T](t: T, name: String = ""): Token[T] = Token.Value(t, name)
    def addToken[T](n: Int, v:Token[T]): Arc = new PartialFunction[Box, Box]:
      override def isDefinedAt(x: Box): Boolean = true
      override def apply(x: Box): Box = box(n,v)
    def moveToken(t:Token[?]): Arc = inArcOf[Any](_ == t)(identity)(identity)(_ => List())
    private def moveAnyToken(): Arc = (o:Box) => o.nonEmpty match
      case true => List(o.head)
    def moveNToken(n: Int): Iterable[Arc] = (0 until n).map(_ => moveAnyToken())




    extension [I](p: I)
      @targetName("Link a place to an ingoing arc")
      def <--(a: Arc): (I, Arc) = p -> a
      @targetName("Move n token from the place p")
      def <--(n: Int): Iterable[(I, Arc)] = moveNToken(n).map(a => p -> a)
      @targetName("Consume a specific token from the place p")
      def <--[T](a: T): (I, Arc) = p -> moveToken(token(a))


    def empty[T: ClassTag]: Arc = inArcOf[T](_ => true)(_ => Token.Empty[T]())(identity)(_ => List())

    @targetName("Create an arc that moves a certain type of token")
    def <>[T: ClassTag]: Arc = inArcOf[T](_ => true)(identity)(identity)(_ => List())
    @targetName("Create an arc that moves a certain type of token with the name s")
    def <>[T: ClassTag](s: String): Arc = inArcOf[T](_.getName == s)(identity)(identity)(_ => List())
    @targetName("Moves a certain type of token after applying a function f to its value")
    def <>[T: ClassTag](f: T => ?): Arc = inArcOf[T](_ => true)(f)(identity)(_ => List())
    @targetName("Moves a certain type of token with name s after applying a function f")
    def :~:[T: ClassTag](s: String)(f: T => ?): Arc = inArcOf[T](_.getName == s)(f)(identity)(_ => List())
    @targetName("Moves a certain type of token mapping its name to s")
    def :~[T: ClassTag](s: String): Arc = inArcOf[T](_ => true)(identity)(_ => s)(_ => List())
    @targetName("Move a token of type T expanding it to a list of tokens using a function f")
    def ~+[T: ClassTag](f:T => Iterable[(?,String)]): Arc = inArcOf[T](_ => true)(identity)(identity)(f)
    @targetName("Move a token of type T and name s expanding it to a list of tokens using a function f")
    def :+[T: ClassTag](f: T => Iterable[(?, String)])(s:String): Arc = inArcOf[T](_.getName == s)(identity)(identity)(f)

    def ?>[T: ClassTag](f: T ?=> Boolean): Arc =
      inArcOf[T](extract[T](_).exists(v => f(using v.getValue.get)))(identity)(identity)(_ => List())

    def ><[T:ClassTag](using m: Box):T = >?[T](_ => true)
    def >?[T: ClassTag](f:String => Boolean)(using m: Box): T =
      m.filter(extract[T](_).isDefined).filter(t => f(t.getName)).map(extract[T](_).get.getValue.get).head
    @targetName("Get the value of the token with name n from the box m")
    def ><[T:ClassTag](n:String)(using m:Box):T = >?[T](_ == n)

    trait Composable[T]:
      def compose(a:T, b:T): T

    @targetName("Group the results of the arcs matching the type T into a single token using the reduction function f")
    def >>[T:ClassTag](arcs:Arc*)(using f: Composable[T]) :Arc = new PartialFunction[Box, Box]:
        override def isDefinedAt(x: Box): Boolean = arcs.forall(_.isDefinedAt(x))
        override def apply(x: Box): Box = box(arcs.flatMap(a => a(x)).map(extract[T](_))
          .filter(_.isDefined).map(_.get).reduce((a,b) =>
            token(f.compose(a.getValue.get,b.getValue.get))))


    extension [I](m: Iterable[(I, Arc)])
      @targetName("Set the condition arcs for a transition")
      def ~~>(name: String): Transition[I,Token[?]] =
        Transition.ofList[I,Token[?]](m, Map[Arc, I](), name, true)
      @targetName("Create a transition with no name")
      def ~~>(m2: Iterable[(Arc, I)]): Transition[I, Token[?]] =
        Transition.ofList[I, Token[?]](m, m2.toMap, "", true)
    extension [I](t: Transition[I,Token[?]])
      @targetName("Set the effect arcs for a transition")
      def ~~>(m: Iterable[(Arc, I)]): Transition[I,Token[?]] =
        Transition.ofMap[I, Token[?]](t.condition, m.toMap, t.name, true)
      @targetName("Add a guard to the transition")
      def ??(f: Box ?=> Boolean):Transition[I,Token[?]]=
        Transition.ofMap[I, Token[?]](t.condition, t.action, t.name, f)
      @targetName("alias for withInhibitorArcs")
      def ^^^(i: Iterable[I]): Transition[I,Token[?]] = InhibitedTransition(t, i.toSet)


    extension (n: Int)
      @targetName("Create an arc that adds n times a certain token")
      def <>[T](v: T): Arc = addToken(n, token(v))
      @targetName("Replicate n times the in-arc v")
      def </>[I](v: (I, Arc)): Iterable[(I,Arc)] = 0.until(n).map(_ => v)
      @targetName("Replicate n times the out-arc v")
      def **[I](v: Arc): Arc = new PartialFunction[Box,Box]:
        override def isDefinedAt(x: Box): Boolean = v.isDefinedAt(x)
        override def apply(x: Box): Box = List.fill(n)(v(x)).reduce(_ ++ _)

      @targetName("Adds n tokens to the place p")
      def +>[I](p: I): (Arc, I) = addToken(n, noValueToken) -> p
    extension (a: Arc)
      @targetName("Link an outgoing arc to a place")
      def -->[I](p: I): (Arc, I) = a -> p
      @targetName("Rename all the generated tokens name of the arc using function f")
      def %(f: Token[?] => String):Arc =
        val p:PartialFunction[Box,Box] = (b:Box) => b.map(t => t.ofName(f(t)))
        a.andThen(p)

    extension [T](v: T)
      @targetName("Add a token to a place")
      def -->[I](p: I): (Arc, I) = addToken(1, token(v)) -> p

    extension [I,T](m:Seq[SystemAnalysis.Path[Marking[I,T]]])
      def prettyPrint:String = m.toList.map(x =>
      x.map(m => s"[${m.map(e => s"${e._1} -> ${e._2.mkString("{",",","}")}")
          .mkString(" , ")}]").mkString(" <> ")).mkString("\n")
      def noTokenPrint:String = m.toList.map(x =>
        x.map(m =>  m.toList.flatMap((p,b) => List.fill(b.size)(p))
            .mkString("[",",","]")).mkString(" <> ")).mkString("\n")

  object BasicPN:
    import PetriNetFacade.PNOperation.*
    export PNOperation.{>, prettyPrint, noTokenPrint, ??}

    def apply[I](t: Transition[I, Token[?]]*): PetriNet[I] =
      PetriNet[I](t.toSet)


    extension [I](cond: Iterable[I])
      @targetName("Create a transition with the given condition cond and effects eff")
      def ~~>(eff: Iterable[I]): Transition[I, Token[?]] =
        val condition = cond.groupMapReduce(identity)(x => 1)
          ((a, b) => a + b).map((i, n) => i -> moveNToken(n))
        val effect = eff.groupMapReduce(identity)(x => 1)
          ((a, b) => a + b).map((i, n) => addToken(n, noValueToken) -> i )
        Transition.ofMap(condition, effect, "",true)

    private def markingOf[I](m:(I,Int)*):Map[I,Box] =
      m.toList.map((i, n) => i -> List.fill(n)(noValueToken)).toMap
    def marking[I](m:I*):Map[I,Box] =
       markingOf(m.toList.counted.toList*)

  object PN:
    export BasicPN.{apply => _, _}
    export PNOperation.^^^

    def apply[I](t: Transition[I, Token[?]]*): PetriNet[I] = BasicPN.apply(t*)

  object CPN:
    export PNetExtensions.InhibitedTransition.*
    export PNOperation.*

    def apply[I](t: Transition[I, Token[?]]*): PetriNet[I] = PetriNet(t.toSet)
    //def marking[I,T](p: (I, Box)*): Map[I, Box] = p.toMap
    def anonMarking[I, T](p: (I, Iterable[T])*): Map[I, Box] = p.map((i, t) => i -> t.map(token(_))).toMap

    def marking[I, T](p: (I, Iterable[(T, String)])*): Map[I, Box] = p.map((i, t) =>
      i -> t.map(a => token(a._1,a._2))).groupMapReduce(_._1)(_._2)(_ ++ _)

  object CPPN:
    export CPN.{apply => _, _}
    export PNetExtensions.PriorityNet.*

    def apply[I](t: (Transition[I, Token[?]], Int)*): PriorityPNet[I] =
      PriorityPNet(t.map((t,p) => t `withPriority` p)*)

    extension [I](t: Transition[I, Token[?]])
      @targetName("Set the priority of the transition")
      def #-#(n: Int): (Transition[I, Token[?]], Int) = t -> n

  object PPN:
    import CPPN.{apply as _, *}
    export PN.{apply => _ , _}
    export PNetExtensions.PriorityNet.*

    def apply[I](t: (Transition[I, Token[?]], Int)*): PetriNet[I] =
      CPPN[I](t*)

    extension [I](t: Transition[I, Token[?]])
      @targetName("Set the priority of the transition")
      def #-#(n: Int): (Transition[I, Token[?]], Int) = CPPN.#-#(t)(n)





