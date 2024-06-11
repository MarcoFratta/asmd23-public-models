package scala.u07.lab

import org.scalatest.funsuite.AnyFunSuite

import scala.u07.utils.MSet
import scala.u07.lab.SPN
import scala.u07.lab.SPN.toCTMC
import scala.u07.modelling.CTMCSimulation
import java.util.Random
import scala.u06.examples.*
import scala.u07.examples.SPn
import scala.u06.modelling.SystemAnalysis

class SPNTest extends AnyFunSuite:

  import CTMCUtils.*



  import CTMCSimulation.*

  test("SPN"):
    enum Place:
      case N, T, C

    import Place.*
    import scala.u07.lab.SPN.*
    val spn = SPN[Place](
      (>(N) ~~> >(T)) ** 1.0,
      (>(T) ~~> >(C)) **(m => m(T) * 1.0),
      >(C) ~~> >() ** 1.0)


    println:
      toCTMC(spn).newSimulationTrace(marking(N, N, N, N), new Random)
        .take(20)
        .toList.mkString("\n")

  test("CPN to SPN"):
    import CPn.Place.*
    import CSPN.*

    val cpn = CPn.comProtocol
    val spn = cpn.toSpn(_ => 1.0, _ => 1.0, _ => 1.0, _ => 1.0, _ => 1.0)

    val m = marking(
      SEND -> >(
        (1 -> "sending") -> "p1",
        (2 -> " a me") -> "p2",
        (3 -> "ssage") -> "p3"),
      NS -> >(1 -> "n"),
      NR -> >(1 -> "n"),
      RECEIVED -> >("" -> "str"),
    )


    println:
      val f = spn.toCTMC
      val k = 0 to 1000 map (_ => f.newSimulationTrace(m, new Random).take(20))
      val avgTime = avgTimeTo(k).andThen(roundAt(3))(m =>
        m(RECEIVED) == >(token("sending a message", "str")))
      val percentageOf = percentageOfTimeIn(k).andThen(roundAt(2))(m =>
        m.applyOrElse(A ,_ => >()) == >(token((1,"sending"), "p1")))

      "avg time to receive all the packets: " + avgTime + "\n" +
        "% of time in receive ack state: " + percentageOf + "%"
      + "\n" + k.last.mkString("\n")

  test("Repressilator"):
    import SPn.Repressilator.*
    import SPn.Repressilator.Place.*
    import SystemAnalysis.*
    import CSPN.*
    val net = SPn.Repressilator.net
    val f = net.toCTMC
    val m = marking[Place,Gene](GENE -> (
      List.fill(10)(1 -> "") ++
      List.fill(10)(2 -> "") ++
      List.fill(10)(3 -> "")))
    val k = 0 to 50 map (_ => f.newSimulationTrace(m, new Random).take(7))
    val avgTime = avgTimeTo(k).andThen(roundAt(5))(m =>
      m.applyOrElse(BLOCKED, _ => >()).toList.contains(token[Gene](2,"x")))
    val percentageOf = percentageOfTimeIn(k).andThen(roundAt(2))(m =>
      m.applyOrElse(BLOCKED, _ => >()).isEmpty)
   // println(net.toSystem.paths(m, 3).prettyPrint)
    println:
      "avg time to BLOCKED contains gene 2: " + avgTime + "\n" +
        "% of time where BLOCKED is empty: " + percentageOf + "%"
        + "\n" + k.last.mkString("\n")

  test("Membrane system"):
    import SPn.MembraneSystems.*
    import SPn.MembraneSystems.Place.*
    import SystemAnalysis.*
    import CSPN.*
    import SPn.MembraneSystems.Token.*
    val net = SPn.MembraneSystems.net
    val f = net.toCTMC
    val m = marking[Place,Token](
      CELL -> List.fill(20)(EXT -> ""),
      RNA  -> List.fill(1)(EXT_ENV_CAP -> ""))
    val k = 0 to 50 map (_ => f.newSimulationTrace(m, new Random).take(20))
    val avgTime = avgTimeTo(k).andThen(roundAt(5))(m =>
      m.applyOrElse(CELL, _ => >()).size <= 10)
    val percentageOf = percentageOfTimeIn(k).andThen(roundAt(2))(m =>
      m.applyOrElse(RNA, _ => >()).nonEmpty)
    // println(net.toSystem.paths(m, 3).prettyPrint)
    println:
      "avg time to CELL <= 10: " + avgTime + "\n" +
        "% of time where RNA is non empty: " + percentageOf + "%"
        + "\n" + k.last.mkString("\n")


