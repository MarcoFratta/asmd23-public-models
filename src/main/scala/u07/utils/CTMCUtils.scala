package scala.u07.utils

import scala.u07.modelling.CTMCSimulation.Trace

object CTMCUtils:


  def avgTimeTo[A](t:Iterable[Trace[A]])(expected:A):Double =
    t.map(_.find(_.state == expected)).filter(_.isDefined).map(_.last.time).sum / t.size

  def percentageOfTime[A](t:Iterable[Trace[A]])(expected:A):Double =
    (t.map(t => t.reverse.foldLeft((t.last.time, 0.0))((a, b) => b.state == expected match
      case false => b.time -> a._2
      case true => b.time -> (a._2 + (a._1 - b.time)))._2)
      .sum / t.map(_.last.time).sum) * 100

  def roundAt(p: Int)(n: Double): Double = {
    val s = math pow(10, p); (math round n * s) / s
  }
