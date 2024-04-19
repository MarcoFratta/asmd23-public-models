package scala.u07.examples

import java.util.Random
import scala.u07.examples.StochasticChannel.*
import scala.u07.lab.CTMCUtils.{avgTimeTo, percentageOfTimeIn}
import scala.u07.utils.Time

@main def mainStochasticChannelSimulation =
  Time.timed:
    println:
      val f =  stocChannel
      val k: Seq[Trace[State]] = 0 to 1000 map (_ => f.newSimulationTrace(IDLE, new Random).take(20))
      "avg time to Done: " + avgTimeTo(k)(_ == DONE) + "\n" +
        "% of time in fail state: " + percentageOfTimeIn(k)(_ == FAIL) + "%"


