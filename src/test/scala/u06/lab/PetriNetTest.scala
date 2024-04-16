package scala.u06.lab

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{be, should}

import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.u06.lab.PetriNetApi.*
import scala.u06.lab.PetriNetFacade.*
import scala.u06.modelling.SystemAnalysis.*



class PetriNetTest extends AnyFunSuite:

  import PetriNetFacade.CPN.*

  test("BasicTest"):

    enum Token:
      case BOOK, PAPER
    enum Place:
      case PC, PN

    import Place.*
    import Token.*

    val pNet = CPN(
      >(PC <-- BOOK) ~~> "T1" ~~> >(BOOK +-> PN)
    ).toSystem
    val m = anonMarking(
      PC -> >(BOOK),
      PN -> box())

    println(pNet.completePathsUpToDepth(m,3).prettyPrint)

  test("MutualExclusionTest"):

    enum Place:
      case N, T, C
    import Place.*

    import PetriNetFacade.PN.*

    def pnME = PN[Place](
      >(N) ~~> >(T),
      >(T) ~~> >(C) ^^^ >(C),
      >(C) ~~> >()
    )

    println(s"Trns -> ${pnME.transitions.size}")

    val m = marking(N -> 2, T -> 0, C -> 0)
    println(s"Marking -> $m")
    val res = pnME.toSystem.paths(m,7)
    println(res.prettyPrint)
    res.size should be(3)



  test("Priority Net test"):
    enum State:
      case RES, OP_A, OP_B, DONE
    import State.*

    import PetriNetFacade.PPN.*
    def net = PPN[State](
      >(RES, RES) ~~> >(OP_A) #-# 1,
      (>(RES) ~~> >(OP_B) ^^^ >(OP_B)) #-# 2,
      >(RES, OP_A, OP_B) ~~> >(RES) #-# 3,
      >(OP_B) ~~> >(DONE) #-# 1
    ).toSystem

    // safety property check
    val paths = net.completePathsUpToDepth(marking(>(RES, OP_A)), 10)
    val res = paths.forall(p => p.last.filterNot((place, tokens) =>
      place == DONE || place == OP_A).forall(_._2.isEmpty))
    println(s"Paths -> ${paths.head.last}")
    res should be:
      true

  test("Communication protocol example"):
    enum Place:
      case SEND, RECEIVED, NS, NR,
        A, B, C, D

    import Place.*

    import PetriNetFacade.CPN.*
    import Token.*


    extract[Int](Value(("ciao",5), "ok")).isDefined should be(false)

    type Packet = (Int,String)
    type Counter = Int

    given stringSum: Composable[String]  = _ + _
    // Tells how a packet should be expanded to its components
    val packet = ~+((p:Packet) => >(p._1 -> "n", p._2 -> "p"))

    val net = CPN[Place](
      >(SEND <-- packet, NS <-- <>[Int]("n")) ~~> "Send packet"
        ~~> >(<>[Packet] --> A, <>[Packet] --> SEND, <>[Int]("n") --> NS),
      >(A <-- <>[Packet] ) ~~> "Transmit packet" ~~> >(<>[Packet] --> B),
      >(B <-- packet, NR <-- <>[Int]("n"), RECEIVED <-- <>[String]("str"))
        ~~> "Receive packet" ~~> >(
          :~:[Int]("n")(_+1) --> C,
          :~:[Int]("n")(_+1) --> NR,
          >>[String](<>[String]("str"), <>[String]("p")) % (_ => "str") --> RECEIVED),
      >(C <-- <>[Int]) ~~> "Transmit ack" ~~> >(<>[Int] --> D),
      >(D <-- <>[Int], NS <-- :~[Int]("k")) ~~> "Receive ack" ~~> >(<>[Int]("n") --> NS),
    )

    val m = marking(
      SEND -> >(
        (1 -> "sending") -> "p1",
        (2 -> " a me") -> "p2",
        (3 -> "ssage") -> "p3"),
      NS -> >(1 -> "n"),
      NR -> >(1 -> "n"),
      RECEIVED -> >("" -> "str"),
    )

    val paths = net.toSystem.paths(m, 14)
    println(s"Marking: \n ${paths.takeRight(1).prettyPrint}")
    val allPacketsReceived = paths.filter(p =>
      p.last.applyOrElse(RECEIVED, _ => >()) == >(token("sending a message", "str")))

    // Can only exists one path with length 14 that ends with the all packets received
    allPacketsReceived.size should be(1)


  test("test binding"):
    enum Place:
      case SEND, RECEIVED, NS, NR,
      A, B, C, D

    import Place.*

    import PetriNetFacade.CPN.*

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



