package scala.u07.lab

import scala.u07.modelling.CTMCSimulation.{Event, Trace}

object CTMCUtils:


  def avgTimeTo[A](t:Iterable[Trace[A]])(filter: A => Boolean):Double =
    val times = t.map(_.find(s => filter(s.state))).filter(_.isDefined).map(_.last.time)
    times.sum / times.size
  
  def percentageOfTimeIn[A](t:Iterable[Trace[A]])(filter: A => Boolean):Double =
    (t.map(t => t.reverse.foldLeft((t.last.time, 0.0))((a, b) => filter(b.state) match
      case false => b.time -> a._2
      case true => b.time -> (a._2 + (a._1 - b.time)))._2)
      .sum / t.map(_.last.time).sum) * 100

  def roundAt(p: Int)(n: Double): Double = {
    val s = math pow(10, p); (math round n * s) / s
  }
