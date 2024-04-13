package scala.u06.modelling


import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.u06.PriorityNetExample



class PriorityNetTest extends AnyFunSuite:


  import scala.u06.modelling.SystemAnalysis.*

  test("PriorityNet"):
    import PriorityNetExample.State.*
    import scala.u06.lab.PetriNetFacade.PPN.*
    val net = PriorityNetExample.net
    val paths = net.completePathsUpToDepth(marking(>(RES,OP_A)), 10)
    val res =paths.forall(p => p.last.forall((_, tokens) =>
        tokens.forall(s => s.equals(DONE) || s.equals(OP_A))))
    res should be:
      true