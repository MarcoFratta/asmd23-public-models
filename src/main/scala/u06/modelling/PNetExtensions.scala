package scala.u06.modelling

import scala.annotation.targetName
import scala.u06.lab.PetriNet.*


object PNetExtensions:


  object InhibitedTransition:
    private trait inhibitorArcs[I, T](i: Set[I]) extends Transition[I, T]:
      abstract override def isEnabled(m: Marking[I, T]): Boolean =
        !m.exists((k, v) => i.contains(k) && v.nonEmpty) && super.isEnabled(m)
          && super.isEnabled(m)

    private case class Trn[I,T](transition: Transition[I,T], i:Set[I])
      extends Transition.TransitionImpl[I,T](transition.condition,
        transition.action,transition.name) with inhibitorArcs[I,T](i)

    def apply[I,T](t: Transition[I,T],i: Set[I]): Transition[I,T] = Trn(t, i)

  object PriorityNet:
    private[PriorityNet] case class PTrn[I, T](t: Transition[I, T], p: Int)
      extends Transition.TransitionImpl[I, T](t.condition,
        t.action, t.name) with Transition[I, T]


    case class PriorityPNet[I,T](transitions:PTrn[I,T]*)

    extension[I,T](t: Transition[I,T])
      def withPriority(p: Int): PTrn[I,T] = PTrn(t,p)

    extension [I, T](p: PriorityPNet[I, T])
      def toSystem: System[Marking[I, T]] = m =>
        //println("Marking : " + m)
        //println("Enabled transitions: " + p.transitions.filter(_.isEnabled(m)))

        def printAndReturn(t: Transition[I, T]) = {
          val r = t.fire(m)
          //println("Before firing: " + m)
          //println("After firing: " + r)
          r
        }
        val enabledTransitions = p.transitions.filter(_.isEnabled(m))
        val maxPriority = enabledTransitions.maxByOption(_.p).getOrElse(0)
        for PTrn(trn,priority) <- enabledTransitions.toSet
          if priority == maxPriority
          out = printAndReturn(trn)
        yield out


