package scala.u08.modelling

import java.util.Random
import scala.u07.modelling.CTMC
import scala.u08.utils.Stochastics

object CTMCSimulation:

  case class Event[A](time: Double, state: A)
  type Trace[A] = LazyList[Event[A]]

  export CTMC.*

  extension [S](self: CTMC[S])
    def newSimulationTrace(s0: S, rnd: Random): Trace[S] =
      LazyList.iterate(Event(0.0, s0)) { case Event(t, s) =>
        if self.transitions(s).isEmpty
        then
          println("emtpy transitions")
          Event(t, s)
        else
          println(s"Getting transitions: ")
          val choices = self.transitions(s) map (t => (t.rate, t.state))
          println("choices size: " + choices.toSeq.size)
         // println("choices: " + choices)
          val next = Stochastics.cumulative(choices.toList)
          println("next: " + next)
          val sumR = next.last._1
          println(s"sum rates $sumR")
          val choice = Stochastics.draw(next)(using rnd)
          println(s"Increasing t ${Math.log(1 / rnd.nextDouble())}")
          Event(t + Math.log(1 / rnd.nextDouble()) / sumR, choice)
      }
