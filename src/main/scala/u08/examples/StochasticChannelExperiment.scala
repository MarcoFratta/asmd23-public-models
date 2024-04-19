package scala.u08.examples

import scala.math.BigDecimal.double2bigDecimal
import scala.u08.utils.Time
import scala.u08.modelling.CTMCExperiment

object StochasticChannelExperiment extends App with de.sciss.chart.module.Charting:
  import CTMCExperiment.*
  import StochasticChannel.*

  val data =
    for
      t <- 0.1 to 10.0 by 0.1
      p = stocChannel.experiment(
        runs = 26000,
        prop = stocChannel.always(s => s == IDLE || s == SEND ||  s == FAIL),
        s0 = IDLE,
      timeBound = t.toDouble)
    yield (t, p)

  Time.timed:
    println:
      data.mkString("\n")

  given ChartTheme = ChartTheme.Default
  val chart = de.sciss.chart.api.XYLineChart(data)
  chart.show("P")