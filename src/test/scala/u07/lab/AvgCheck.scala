package scala.u07.lab

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{should,be}


import scala.u07.modelling.CTMCSimulation.Event

class AvgCheck extends AnyFunSuite:

  import CTMCUtils.*
  import scala.u06.lab.PetriNetFacade.CPN.*
  import scala.u06.lab.PetriNetFacade.PNOperation.token


  enum Place:
    case A, B, C

  import Place.*

  test("Avg test with one trace"):
    val trace = LazyList(Event(1.0, A), Event(2.0, B), Event(3.0, C), Event(6.0, A))
    val timeTo = avgTimeTo(List(trace)).andThen(roundAt(1))
    timeTo(_ == B) should be(2.0)
    timeTo(_ == C) should be(3.0)

  test("Avg test with two traces"):
    val trace1 = LazyList(Event(1.0, A), Event(2.0, B), Event(3.0, C), Event(6.0, A))
    val trace2 = LazyList(Event(1.0, A), Event(5.0, B), Event(8.0, C), Event(10.0, B))
    val timeTo = avgTimeTo(List(trace1, trace2)).andThen(roundAt(1))
    timeTo(_ == B) should be(3.5)
    timeTo(_ == C) should be(5.5)

  test("Avg test with two traces with a map as marking"):
    val trace = LazyList(
      Event(1.0, anonMarking(A -> >(1), B -> >(), C -> >())),
      Event(3.0, anonMarking(A -> >(), B -> >(1), C -> >())),
      Event(4.0, anonMarking(A -> >(), B -> >(), C -> >(1))))

    val trace2 = LazyList(
      Event(0.5, anonMarking(A -> >(1), B -> >(), C -> >())),
      Event(2.0, anonMarking(A -> >(), B -> >(1), C -> >())),
      Event(6.0, anonMarking(A -> >(2), B -> >(), C -> >(1))))

    val timeTo = avgTimeTo(List(trace, trace2)).andThen(roundAt(1))
    timeTo(s => s(B) == >(token(1)))should be(2.5)
    timeTo(s => s(C) == >(token(1))) should be(5.0)
    timeTo(s => s(A) == >(token(2))) should be(6.0)