package scala.u06.lab

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.u06.lab.PetriNetApi.{Box, Token, box}
import scala.u06.lab.PetriNetFacade.CPN.{anonMarking, marking}

class ArcTest extends AnyFunSuite:
  import PetriNetFacade.PNOperation.*
  private enum MyToken:
    case Value, Empty
  import MyToken.*

  test("An arc should not be enabled if the condition is not satisfied"):

    token(Value) == token(Value) shouldBe true
    val arc = moveToken(token(Value))
    arc.isDefinedAt(box(token(Empty))) shouldBe false
    arc.isDefinedAt(box(token(Value))) shouldBe true


  test("Move n token should work") {
    val arc = moveNToken(3)
    val b = box(3, token(Value))
    arc.forall(_.isDefinedAt(box(token(Value)))) shouldBe true
    arc.flatMap(_(b)) shouldBe >(token(Value), token(Value), token(Value))

    val arc2 = moveNToken(2)
    arc2.flatMap(_(box(noValueToken))) shouldBe >(noValueToken, noValueToken)
  }

  test("Move a token of type T should work"):
    val arc = ~+((p:(Int,String)) => >(p._1 -> "n", p._2 -> "p"))
    val t =
    arc.isDefinedAt(box(token((5, "pizza")))) shouldBe true
    arc(box(token((5, "pizza")))) shouldBe  >(token(5, "n"), token("pizza", "p"), token((5,"pizza")))

  test("Type extractor should work"):
    val arc = <>[String]
    arc.isDefinedAt(box(token("pizza"))) shouldBe true
    arc.isDefinedAt(box(token(3))) shouldBe false
    println(arc(box(token(3.0), token("ok"))))
    arc.isDefinedAt(box(token(3.0), token("ok"))) shouldBe true
