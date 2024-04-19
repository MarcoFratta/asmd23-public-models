package scala.u06.lab

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.u06.examples.BasicPn
import scala.u06.lab.PetriNetFacade.BasicPN
import scala.u06.modelling.SystemAnalysis
class BasicPnTest extends AnyFunSuite:
  import BasicPn.*
  import BasicPn.Place.*
  import SystemAnalysis.*
  import BasicPN.*
  test("test paths") {
    val paths = pn.paths(marking(IDLE), 3)
    println(paths.prettyPrint)
    paths.size should be(3)
  }
  
