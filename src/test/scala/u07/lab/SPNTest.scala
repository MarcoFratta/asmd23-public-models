package scala.u07.lab

import org.scalatest.funsuite.AnyFunSuite

import scala.u07.utils.MSet
import scala.u07.lab.SPN
import scala.u07.lab.SPN.toCTMC
import scala.u07.modelling.CTMCSimulation
import java.util.Random
import scala.u06.lab.Examples


class SPNTest extends AnyFunSuite:

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
      toCTMC(spn).newSimulationTrace(marking(>(N, N, N, N)), new Random)
        .take(20)
        .toList.mkString("\n")

  test("CPN to SPN"):
    import Examples.ComPlace.*
    import scala.u06.lab.PetriNetFacade.CPN.marking

    val cpn = Examples.comProtocol
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
      toCTMC(spn).newSimulationTrace(m, new Random)
        .take(5)
        .toList.mkString("\n")


