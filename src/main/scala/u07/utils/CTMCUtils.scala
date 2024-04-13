package scala.u07.utils

import u07.modelling.CTMCSimulation.Trace

object CTMCUtils:


  def avgTimeTo[A](t:Iterable[Trace[A]])(expected:A):Double =
    t.map(_.find(_.state == expected)).filter(_.isDefined).map(_.last.time).sum / t.size

  def percentageOfTime[A](t:Iterable[Trace[A]])(expected:A):Double =
    val x = t.map(_.reverse.foldLeft((0.0,0.0))((a,b) => b.state == expected match
      case false => b.time -> a._2
      case true => b.time -> (a._2 + a._1 - b.time))).map(_._2)
    val totalTime = t.map(_.last.time).sum
    (x.sum / totalTime) * 100