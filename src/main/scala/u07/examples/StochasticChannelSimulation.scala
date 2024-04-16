package scala.u07.examples

import scala.u07.utils.Time

import java.util.Random
import scala.u07.examples.StochasticChannel.*
import scala.u07.modelling.SPN.toCTMC

import scala.u06.utils.MSet
import scala.u07.utils.CTMCUtils.{avgTimeTo, percentageOfTime, roundAt}

@main def mainStochasticChannelSimulation =
  Time.timed:
    println:
      val f =  stocChannel
      val k: Seq[Trace[State]] = 0 to 1000 map (_ => f.newSimulationTrace(IDLE, new Random).take(20))
      "avg time to Done: " + avgTimeTo(k)(DONE) + "\n" +
        "% of time in fail state: " + percentageOfTime(k)(FAIL) + "%"


