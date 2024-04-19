package scala.u06.lab

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.u06.modelling.SystemAnalysis
import scala.u06.examples.PPn
import scala.u06.lab.PetriNetFacade.PPN

class PPNTest extends AnyFunSuite:

  test("PriorityNet"):
    import PPn.State.*
    import PPn.*
    import scala.u06.lab.PetriNetFacade.PPN.*
    import SystemAnalysis.*

    val net = PPn.net
    val paths = net.paths(marking(RES, OP_A), 10)
    val res = paths.forall(p => p.last.forall((_, tokens) =>
      tokens.forall(s => s.equals(DONE) || s.equals(OP_A))))
    res should be:
      true
