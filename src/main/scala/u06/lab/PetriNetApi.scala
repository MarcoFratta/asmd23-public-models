package scala.u06.lab

import scala.reflect.ClassTag
import scala.u06.modelling.System

object PetriNetApi:
  enum Token[T](content: T):
    case Value(content: T, name: String) extends Token[T](content)
    case Empty() extends Token[T](null.asInstanceOf[T])
    override def toString: String = this match
      case Token.Value(content, n) => if n.isEmpty then content.toString else s"$n: $content"
      case Token.Empty() => ""

  import Token.*
  extension [T: ClassTag](t: Token[T])
    def getValue: Option[T] = t match
      case Value(content: T, _) => Some[T](content)
      case _ => Option.empty[T]
  extension (t: Token[?])
    def getName: String = t match
      case Value(_, n) => n
      case Empty() => ""
    def ofName(s:String):Token[?] = t match
      case Value(v, n) => Value(v,s)
      case Empty() => Empty()

  type Box = Iterable[Token[?]]
  type Marking[I,T] = Map[I, Box]
  type Cond = Token[?]
  type Arc = PartialFunction[Box, Box]

  def box():Box = List()
  def box(v:Token[?]*):Box = v.toList
  def box(v:Iterable[Token[?]]):Box = v
  def box(n:Int, v:Token[?]):Box = List.fill(n)(v)
  extension[I](b: List[I])
    def counted: Map[I, Int] = b.groupBy(identity).map((a,n) => a -> n.size)

  trait Transition[I,T]:
    def condition: Map[I, Iterable[Arc]]
    def action: Map[Arc, I]
    def isEnabled(m: Marking[I,T]): Boolean
    def fire(m: Marking[I,T]): Marking[I,T]
    def name: String


  case class PetriNet[I](transitions: Set[Transition[I,Token[?]]])

  def apply[I](t: Set[Transition[I,Token[?]]]): PetriNet[I] =
    PetriNet(t)
  def apply[I](t: Transition[I,Token[?]]*): PetriNet[I] =
    PetriNet(t.toSet)

  extension [I](p: PetriNet[I])
    def toSystem: System[Marking[I,Token[?]]] = m =>
      println(s"Enabled transitions: ${p.transitions.filter(_.isEnabled(m)).mkString(", ")}" + s" for marking ${m}")
      for t <- p.transitions
          if t.isEnabled(m)
      yield t.fire(m)

  object Transition:

    class TransitionImpl[I, T](override val condition: Map[I, Iterable[Arc]],
                                            override val action: Map[Arc, I],
                                            override val name: String) extends Transition[I, T]:
      override def toString: String = name match
        case "" => "T" + (condition.hashCode() + action.hashCode()) % 100
        case _ => name

      private def combinationList[A](ls: List[List[A]]): List[List[A]] = ls match {
        case Nil => Nil :: Nil
        case head :: tail => val rec = combinationList[A](tail)
          rec.flatMap(r => head.map(t => t :: r))
      }
      private def checkBindings(arcs :Map[Arc, Box]):
      Iterable[Map[Arc, Token[?]]] =
        case class ArcToken(token: Token[?], box: Box)
        println(s"valid arcs: ${arcs}")
        val tokensAfterFire = arcs.map((a, tokens) => a -> tokens.toList.map(t=> ArcToken(t, a(List(t)))))
        println(s"tokens after fire: ${tokensAfterFire}")
        val permutations = combinationList(tokensAfterFire.toSeq.map((a,b) => b.map(a -> _)).toList)
        println(s"permutations: ${permutations.mkString("\n")}")
        permutations.filter(p =>
          val grouped = p.flatMap((a, arcToken) => arcToken.box
              .filter(_.getName.nonEmpty).toSeq).groupBy(_.getName)
          println(s"grouped: ${grouped}")
            grouped.forall((_,tokens) => tokens.size match
              case 1 => true
              case n => tokens.distinct.size != n
              )).map(perm => perm.map((a, arcToken) =>
          a -> arcToken.token).toMap)

      private def getValidArcs(m: Marking[I, T]): Map[Arc, Box] =
        println(condition)
        val e = condition.toList.flatMap((p, arcs) => arcs.map(p -> _))
        println(s"e: ${e}")
          e.map((p, a) =>
            val tokens = m.getOrElse(p, box())
            println(s"tokens of a ${a}: " + tokens)
            a -> tokens.filter(t => a.isDefinedAt(List(t)))).toMap

      override def isEnabled(m: Marking[I, T]): Boolean =
        // tutti le combinazioni arco -> lista di token cui è definito
        val validArcs = getValidArcs(m)
        println(s"valid arcs: ${validArcs}")
        val c = checkBindings(validArcs)
        println(s"checkbindings: ${c}")
        validArcs.count((_, b) => b.nonEmpty) == condition.size &&
          c.nonEmpty

      override def fire(m: Marking[I, T]): Marking[I, T] =
        // assuming that all the arcs are valid (have at least one token that can fire the arc)
        val validArcs = getValidArcs(m)
        val tokenToFire = checkBindings(validArcs).head //replace with random
        val afterCondition = condition.map((p, arcs) =>
          p -> arcs.flatMap(a => a(List(tokenToFire(a)))))
        val tokensToRemove = condition.map((p, arcs) =>
          p -> arcs.flatMap(a => Seq(tokenToFire(a))))
        val newInPlaces = m.map((place, res) => condition.isDefinedAt(place) match
          case true => place -> res.toSeq.diff(tokensToRemove(place).toSeq)
          case false => place -> res)
        val tokens = afterCondition.values.flatten.partition(_.getName.isEmpty)
        val distinctTokens = tokens._1.toSeq ++ tokens._2.toSeq.distinctBy(_.getName)
        println(s"tokens: ${tokens._1} ${tokens._2}")
        println(s"distinct tokens: ${distinctTokens}")
        println(s"new places: ${newInPlaces}")
        val newOutPlaces = action.map((a, p) =>
          p -> newInPlaces.getOrElse(p, box()).concat(a.applyOrElse(distinctTokens, _ =>
          throw IllegalStateException(s"An out arc of transition ${this.toString} cannot be applied with" +
            s" the tokes taken from InArcs")))) // can fail
        println(s"new out places: ${newOutPlaces}")
        val out = newInPlaces ++ newOutPlaces
        println(s"Firing -> ${this.name}, result:\n\t${m}\n\t$out")
        out

    def apply[I,T](): Transition[I,T] = TransitionImpl(Map(), Map(), "")
    def apply[I,T](name: String): Transition[I,T] = TransitionImpl(Map(), Map(), name)
    def ofMap[I,T](condition: Map[I, Iterable[Arc]], action: Map[Arc, I], name: String): Transition[I,T] =
      TransitionImpl(condition, action, name)
    def ofList[I, T](condition: Iterable[(I, Arc)], action: Map[Arc, I], name: String): Transition[I, T] =
      TransitionImpl(condition.groupMapReduce(_._1)(v => Seq(v._2))((x,y) => x ++ y),
                    action,name)
