package scala.u06.lab

import scala.reflect.ClassTag
import scala.u06.modelling.System
import scala.u07.utils.MSet

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
    def fire(m: Marking[I,T]): Iterable[Marking[I,T]]
    def name: String


  case class PetriNet[I](transitions: Iterable[Transition[I,Token[?]]])

  def apply[I](t: Transition[I,Token[?]]*): PetriNet[I] =
    PetriNet(t.toSet)

  extension [I](p: PetriNet[I])
    def toSystem: System[Marking[I,Token[?]]] = m =>
      (for t <- p.transitions
          if t.isEnabled(m)
      yield t.fire(m).toSeq.distinct).flatten.toSet

  object Transition:

    class TransitionImpl[I, T](override val condition: Map[I, Iterable[Arc]],
                                            override val action: Map[Arc, I],
                                            override val name: String,
                               guard: Box ?=> Boolean) extends Transition[I, T]:
      override def toString: String = name match
        case "" => "T" + (condition.hashCode() + action.hashCode()) % 100
        case _ => name

      private def combinationList[A](ls: List[List[A]]): List[List[A]] = ls match {
        case Nil => Nil :: Nil
        case head :: tail =>
          val rec = combinationList[A](tail).distinct
          rec.flatMap(r => head.map(t => t :: r))
      }

      private def checkBindings(arcs :List[(Arc, Box)]): Iterable[Map[Arc, Token[?]]] =
        case class ArcToken(token: Token[?], box: Box)
        val tokensAfterFire = arcs.map((a, tokens) => a -> tokens.toList.map(t=> ArcToken(t, a(List(t)))))
        //println("Computing combinations")
        val combinations = combinationList(tokensAfterFire.map((a, b) => b.map(a -> _)))
        //println("Finished computing " + combinations.size)
        combinations.filter(p => p.flatMap((a, arcToken) => arcToken.box
              .filter(_.getName.nonEmpty).toSeq).groupBy(_.getName).forall((_,tokens) => tokens.size match
              case 1 => true
              case n => tokens.forall(_ == tokens.head)
              )).map(perm => perm.map((a, arcToken) =>
          a -> arcToken.token).toMap)

      private def getValidArcs(m: Marking[I, T]): List[(Arc, Box)] =
        condition.toList.flatMap((p, arcs) => arcs.map(p -> _)).map((p, a) =>
          val tokens = m.getOrElse(p, box())
          a -> tokens.filter(t => a.isDefinedAt(List(t))))

      private def possibleScenarios(marking: Marking[I,T],
                                    places: Map[I, Iterable[Arc]],
                                    bindings:Iterable[Map[Arc,Token[?]]]):Iterable[Map[Arc,Token[?]]] =
        val invalidScenarios =
          for scenario <- bindings
              p <- places.keys
              needed = places(p).map(a => scenario(a))
              b = marking.getOrElse(p, box())
              if !MSet.ofList(b.toList).matches(MSet.ofList(needed.toList))
              yield scenario
        bindings.toList diff invalidScenarios.toSeq


      override def isEnabled(m: Marking[I, T]): Boolean =
        // tutti le combinazioni arco -> lista di token cui è definito
        val validArcs = getValidArcs(m)
        println("getting bindings")
        val c = checkBindings(validArcs)
        println("finished " + c.size)
        val scenarios = possibleScenarios(m, condition, c)
        val validScenarios = scenarios.filter(s => guard(using s.flatMap((a, t) => a(List(t)))))
//        println(s"Valid scenarios: $validScenarios")
//        println(s"Valid arcs: $validArcs")
//        println(s"Bindings: $c")
        validArcs.count((_, b) => b.nonEmpty) == condition.flatMap(_._2).size
          && validScenarios.nonEmpty


      override def fire(m: Marking[I, T]): Iterable[Marking[I, T]] =
        // assuming that all the arcs are valid (have at least one token that can fire the arc)
        val validArcs = getValidArcs(m)
        val bindings = possibleScenarios(m, condition, checkBindings(validArcs))
        val validScenarios = bindings.filter(s => guard(using s.flatMap((a, t) => a(List(t))))).toSeq
        println(s"Valid scenarios: ${validScenarios.size}")
        for scenario <- validScenarios
          afterCondition = condition.map((p, arcs) =>
            p -> arcs.flatMap(a => a(List(scenario(a)))))
          tokensToRemove = condition.map((p, arcs) =>
            p -> arcs.flatMap(a => Seq(scenario(a))))
          newInPlaces = m.map((place, res) => condition.isDefinedAt(place) match
            case true => place -> res.toSeq.diff(tokensToRemove(place).toSeq)
            case false => place -> res)
          distinctTokens = afterCondition.values.flatten
//          println(distinctTokens)
//          println(action)
          newOutPlaces = action.toList.map((a, p) =>
            p -> newInPlaces.getOrElse(p, box()).concat(a.applyOrElse(distinctTokens, _ =>
            throw IllegalStateException(s"An out arc of transition ${this.toString} cannot be applied with" +
              s" the tokes taken from InArcs")))).groupMapReduce(_._1)(_._2)((b1,b2) => b1 ++ b2) // can fail
          //println(newOutPlaces)
          yield newInPlaces ++ newOutPlaces

    def apply[I,T](): Transition[I,T] = ofMap(Map(), Map(), "", true)
    def apply[I,T](name: String): Transition[I,T] = ofMap(Map(), Map(), name,true)
    def ofMap[I,T](condition: Map[I, Iterable[Arc]], action: Map[Arc, I], name: String,
                   g: Box ?=> Boolean): Transition[I,T] = TransitionImpl(condition, action, name,g)
    def ofList[I, T](condition: Iterable[(I, Arc)], action: Map[Arc, I], name: String,
                     g: Box ?=> Boolean): Transition[I, T] =
      TransitionImpl(condition.groupMapReduce(_._1)(v => Seq(v._2))((x,y) => x ++ y),
                    action,name, g)
