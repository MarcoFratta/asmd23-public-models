package scala.u08.examples

import scala.math.BigDecimal.double2bigDecimal
import scala.u06.lab.PetriNetApi.{Box, Marking}
import scala.u06.lab.PetriNetFacade.CPN.marking
import scala.u06.lab.PetriNetFacade.PNOperation.token
import scala.u07.lab.CSPN.*
import scala.u08.modelling.CTMCExperiment
import scala.u08.utils.Time



object SPnExp extends App with de.sciss.chart.module.Charting:
  import CTMCExperiment.*
  import scala.u07.examples.SPn.MembraneSystems.*
  import scala.u07.examples.SPn.MembraneSystems.Place.*
  import scala.u07.examples.SPn.MembraneSystems.Token.*
  given ChartTheme = ChartTheme.Default
  val pNet = net.CPNtoCTMC
  val m = marking[Place, Token](
    CELL -> List.fill(20)(EXT -> ""),
    RNA -> List.fill(10)(EXT_ENV_CAP -> ""))

  printSim()

  private def printSim():Unit =
    val bound = 10
    for _ <- 0 until 1 by 1 do
        val data = pNet.analyze(bound, m, { v =>
          v.applyOrElse(RNA, _ => Seq()).size ->
          v.applyOrElse(PROTEIN_C, _ => Seq()).size})
        val s1 = "TOTAL RNA" -> data.map((a,b) => a -> b._1)
        val s2 = "TOTAL C" -> data.map((a,b) => a -> b._2)
        //s2._2.foreach(println)
        val chart = XYLineChart(s1 :: s2 :: Nil)
        chart.show("P")



  private def printP(): Unit = {
    val g = pNet.always[Marking[Place, Box]](s =>
      s.applyOrElse(PROTEIN_C, _ => Seq()).count(_ == token(EXT_INT)) < 15)
    val data =
      for
        t <- 50 until 51 by 1
        p = pNet.experiment(runs = 100,
          prop = g,
          s0 = m,
          timeBound = t.toDouble)
      yield (t, p)

    Time.timed:
      println:
        data.mkString("\n")
    val chart = XYLineChart(data)
    chart.show("P")
  }
