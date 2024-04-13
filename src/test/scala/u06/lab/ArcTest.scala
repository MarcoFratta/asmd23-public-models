package scala.u06.lab

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.u06.lab.PetriNet.{Box, Token}
import scala.u06.lab.PetriNetFacade.CPN.{anonMarking, marking}

class ArcTest extends AnyFunSuite:
  import PetriNetFacade.PNOperation.*
  private enum MyToken:
    case Value, Empty
  import MyToken.*

  test("An arc should not be enabled if the condition is not satisfied"):

    val arc = "S1" <-- Value
    arc._2.isDefinedAt(token(Empty)) shouldBe false
    arc._2.isDefinedAt(token(Value)) shouldBe true


  test("Move n token should work") {
    val arc = moveNToken(3)
    arc(noIdToken) shouldBe (>(), >(noIdToken, noIdToken, noIdToken))

    val arc2 = moveNToken(2)
    arc2(noIdToken) shouldBe(>(), >(noIdToken, noIdToken))
  }

  test("Move a token of type T should work"):
    val arc = ~+((p:(Int,String)) => >(p._1 -> "n", p._2 -> "p"))
    val t =
    arc.isDefinedAt(token((5, "pazza"))) shouldBe true
    arc(token((5, "pazza"))) shouldBe (>(), >(token(5, "n"), token("pazza", "p"), token((5,"pazza"))))

