package scala.u06.lab

import scala.annotation.targetName
import scala.u06.lab.PetriNetApi.*
import scala.u06.modelling.System


object PNetExtensions:


  object InhibitedTransition:
    private trait inhibitorArcs[I, T](i: Iterable[I]) extends Transition[I, T]:
      abstract override def isEnabled(m: Marking[I, T]): Boolean =
        !m.exists((k, v) => i.toSeq.contains(k) && v.nonEmpty) && super.isEnabled(m)

    private case class Trn[I,T](transition: Transition[I,T], i:Set[I])
      extends Transition.TransitionImpl[I,T](transition.condition,
        transition.action,transition.name, true) with inhibitorArcs[I,T](i)

    def apply[I,T](t: Transition[I,T],i: Set[I]): Transition[I,T] = Trn(t, i)

  object PriorityNet:
    private[PriorityNet] case class PTrn[I, T](t: Transition[I, T], p: Int)
      extends Transition.TransitionImpl[I, T](t.condition,
        t.action, t.name, true)


    class PriorityPNet[I](val pTransitions: PTrn[I,Token[?]]*)
      extends PetriNet[I](pTransitions.map(_.t))

    extension[I,T](t: Transition[I,T])
      def withPriority(p: Int): PTrn[I,T] = PTrn(t,p)

    extension [I, T](p: PriorityPNet[I])
      def toSystem: System[Marking[I, T]] = m =>
        val enabledTransitions = p.pTransitions.filter(_.isEnabled(m))
        val maxPriority:Int = enabledTransitions.maxByOption(_.p).map(_.p).getOrElse(0)
        (for PTrn(trn,priority) <- enabledTransitions
          if priority == maxPriority
          yield trn.fire(m)).flatten.toSet
     


