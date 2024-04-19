package scala.u06.lab

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.u06.examples.CPn
import scala.u06.lab.PetriNetApi.box
import scala.u06.lab.PetriNetFacade.CPN
import scala.u06.modelling.SystemAnalysis

class CPNTest extends  AnyFunSuite:
  import CPN.*
  import CPn.*
  import CPn.Place.*
  import SystemAnalysis.*

  test("Communication protocol")
    val m = marking(
      SEND -> >(
        (1 -> "sending") -> "p1",
        (2 -> " a me") -> "p2",
        (3 -> "ssage") -> "p3"),
      NS -> >(1 -> "n"),
      NR -> >(1 -> "n"),
      RECEIVED -> >("" -> "str"),
    )

    val paths = comProtocol.toSystem.paths(m, 14)
    println(s"Marking: \n ${paths.takeRight(1).prettyPrint}")
    val allPacketsReceived = paths.filter(p =>
      p.last.applyOrElse(RECEIVED, _ => >()) == >(token("sending a message", "str")))

    // Can only exists one path with length 14 that ends with the all packets received
    allPacketsReceived.size should be(1)

  test("test binding"):

    type Packet = (Int, String)
    type Counter = Int

    val net = CPN[Place](
      >(NS <-- :~[Int]("k"), D <-- <>[Int])
        ~~> "Receive ack" ~~>
        >(<>[Int]("n") --> NS),
    )
    println(net.transitions.head.condition)
    println(net.transitions.head.action)

    val m = marking(
      NS -> >(1 -> "n"),
      D -> >(2 -> "n")
    )

    val expectedMarking = marking(
      NS -> >(2 -> "n"),
      D -> >())

    val paths = net.toSystem.paths(m, 2)
    println(s"Marking-> ${paths.prettyPrint}")
    paths.last.last should be(expectedMarking)

  test("CPN double arc test with id tokens"):
    enum Token:
      case BOOK, PAPER
    enum Place:
      case A, B, C

    import Place.*

    import CPN.*

    val pNet = CPN[Place](
      >(A <-- <>[Int]("n"), A <-- <>[Int], B <-- <>[Int]("n"))
        ~~> "T1" ~~> >(<>[Int]("n") --> C)
    ).toSystem
    val m = marking(
      A -> >(1 -> "p", 2 -> "n"),
      B -> >(2 -> "n"))

    val expected = marking(A -> >(), B -> >(), C -> >(2 -> "n"))
    val res = pNet.paths(m, 2)
    println(res.prettyPrint)
    res.last.last should be(expected)

    val m2 = marking(A -> >(1 -> "", 2 -> ""), B -> >(2 -> "n"))

    val expected2 = marking(A -> >(), B -> >(), C -> >(2 -> "n"))

    val res2 = pNet.paths(m2, 2)
    println(res.prettyPrint)
    res.last.last should be(expected2)

  test("Double arc CPN test with no id tokens"):
    enum Place:
      case A, B, C
    enum Token:
      case BOOK, PAPER

    import Place.*
    import Token.*

    import CPN.*
    val pNet = CPN(
      (A <-- 2) ~~> >(2 --> B)
    ).toSystem

    val m = anonMarking(
      A -> box(2, noIdToken(BOOK)),
      B -> box())

    val res = pNet.paths(m, 2)
    println(res.size)
    println(res.prettyPrint)
    res.size should be(1)

    val m2 = anonMarking(
      A -> box(1, noIdToken(BOOK)),
      B -> box())
    val res2 = pNet.paths(m2, 2)
    res2.size should be(0)

  test("Move same packet"):
    enum Place:
      case A, B, C

    import Place.*

    import CPN.*

    val pNet = CPN[Place](
      >(A <-- <>[Int]("n"),
        A <-- <>[Int]("n"))
        ~~> >(<>[Int]("n") --> B,
              <>[Int]("n") --> B)
    ).toSystem
    val m = marking(
      A -> >(2 -> "n", 2 -> "n"),
      B -> >())

    val expected = marking(A -> >(), B -> >(2 -> "n", 2 -> "n"))
    val res = pNet.paths(m, 2)
    println(res.prettyPrint)
    res.last.last should be(expected)
