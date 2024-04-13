package u07.examples

import u07.utils.Time

import java.util.Random
import u07.examples.StochasticChannel.*
import u07.modelling.SPN.toCTMC

import scala.u06.utils.MSet
import scala.u07.utils.CTMCUtils.{avgTimeTo, percentageOfTime}

@main def mainStochasticChannelSimulation =
  Time.timed:
    println:
      val f =  stocChannel
      val k: Seq[Trace[State]] = 0 to 500 map (_ => f.newSimulationTrace(IDLE, new Random).take(20))
      k.head.mkString("\n")+ "\n" +
        avgTimeTo(k)(DONE) + "\n" +
        percentageOfTime(k)(FAIL)


