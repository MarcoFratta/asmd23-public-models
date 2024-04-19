package scala.u06.lab

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.u06.examples.CPPn
import scala.u06.lab.PetriNetApi.box
import scala.u06.lab.PetriNetFacade.CPN
import scala.u06.modelling.SystemAnalysis

class CPPNTest extends AnyFunSuite:

  import scala.u06.lab.PetriNetFacade.CPPN.*
  import CPPn.*
  import CPPn.State.*
  import CPPn.Token.*
  import SystemAnalysis.*

  test("test cppn"):
    val net = CPPn.net
    val m = marking(P1 -> >(A -> "",B -> ""))
    val expected = marking(
      P1 -> >(),
      P3 -> >())
    val paths = net.paths(m, 3)
    println(paths.prettyPrint)
    paths.last.last should contain theSameElementsAs expected