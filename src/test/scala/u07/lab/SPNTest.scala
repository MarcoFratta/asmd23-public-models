package scala.u07.lab

import org.scalatest.funsuite.AnyFunSuite

import scala.u07.utils.MSet
import scala.u07.lab.SPN
import scala.u07.lab.SPN.toCTMC
import scala.u07.modelling.CTMCSimulation
import java.util.Random
import scala.u06.examples.*
import scala.u06.lab.PetriNetFacade.PNOperation.token

class SPNTest extends AnyFunSuite:

  import CTMCUtils.*
  enum Place:
    case N, T, C
  import Place.*
  import scala.u07.lab.SPN.*
  import CTMCSimulation.*

  test("SPN"):
    val spn = SPN[Place](
      (>(N) ~~> >(T)) * 1.0,
      (>(T) ~~> >(C)) *(m => m(T) * 1.0),
      >(C) ~~> >() * 1.0)


    println:
      toCTMC(spn).newSimulationTrace(marking(N, N, N, N), new Random)
        .take(20)
        .toList.mkString("\n")

  test("CPN to SPN"):
    import scala.u06.lab.PetriNetFacade.CPN.marking
    import CPn.Place.*
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



