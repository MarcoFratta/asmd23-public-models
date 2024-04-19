package scala.u07.lab

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import scala.u07.modelling.CTMCSimulation.Event

class PercentageCheck extends AnyFunSuite:
  import CTMCUtils.*


  enum Place:
    case A, B, C

  import Place.*
  test("Round should work") {
    val n = 1.23456789
    roundAt(2)(n) should be(1.23)
    roundAt(3)(n) should be(1.235)
    roundAt(4)(n) should be(1.2346)
  }

  test("Percentage check of one trace") {
    val trace = LazyList(Event(1.0, A), Event(2.0, B), Event(3.0, C), Event(6.0, A))
    val percentageOf = percentageOfTimeIn(List(trace)).andThen(roundAt(1))
    percentageOf(_ == B) should be(16.7)
    percentageOf(_ == C) should be(50.0)
  }

  test("Percentage check of one trace with small time") {
    val trace = LazyList(
      Event(0.7917517757322000, A),
      Event(0.7917517757322015, B),
      Event(0.7917517757322200, C),
      Event(0.7917517757322250, A))
    val percentageOf = percentageOfTimeIn(List(trace))
    percentageOf(_ == B) should be (2.3365909072E-12 +- 0.01)
  }

  test("Percentage check of one trace where the place is visited twice") {
    val trace = LazyList(Event(1.0, A), Event(2.0, B), Event(3.0, A), Event(6.0, B))
    val percentageOf = percentageOfTimeIn(List(trace)).andThen(roundAt(1))
    percentageOf(_ == A) should be(66.7)
  }
  test("Percentage check of one trace where the place is never visited") {
    val trace = LazyList(Event(1.0, A), Event(2.0, B))
    val percentageOf = percentageOfTimeIn(List(trace)).andThen(roundAt(1))
    percentageOf(_ == C) should be(0.0)
  }

  test("Percentage check of one trace where the place is visited 2 times consecutively") {
    val trace = LazyList(Event(1.0, A), Event(2.0, B), Event(2.5, B), Event(4, A))
    val percentageOf = percentageOfTimeIn(List(trace)).andThen(roundAt(1))
    percentageOf(_ == B) should be(50.0)
  }

  test("Percentage check of one trace where the place is the last one") {
    val trace = LazyList(Event(1.0, A), Event(2.0, B), Event(2.5, B), Event(4, A))
    val percentageOf = percentageOfTimeIn(List(trace)).andThen(roundAt(1))
    percentageOf(_ == A) should be(25.0)
  }

  test("Percentage check of two traces") {
    val trace1 = LazyList(Event(1.0, A), Event(2.0, B), Event(3.0, C), Event(6.0, A))
    val trace2 = LazyList(Event(2.0, A), Event(3.0, B), Event(5.0, C), Event(8.0, A))

    val percentageOf = percentageOfTimeIn(List(trace1, trace2)).andThen(roundAt(1))
    percentageOf(_ == B) should be(21.4)
    percentageOf(_ == C) should be(42.9)
  }

  test("Percentage check of two traces with one where the place is never visited") {
    val trace1 = LazyList(Event(1.0, A), Event(2.0, B), Event(3.0, C), Event(6.0, A))
    val trace2 = LazyList(Event(2.0, A), Event(5.0, C), Event(18.0, A))

    val percentageOf = percentageOfTimeIn(List(trace1, trace2)).andThen(roundAt(1))
    percentageOf(_ == B) should be(4.2)
  }