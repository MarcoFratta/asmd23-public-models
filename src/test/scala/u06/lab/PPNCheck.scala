package scala.u06.lab

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.u06.examples.PPn
import scala.u06.lab.{PPNTest, PetriNetFacade}
import scala.u06.utils.MSet
object PPNCheck extends Properties("PriorityNet"):

  import PPn.State
  import PetriNetFacade.PPN.*
  import scala.u06.modelling.SystemAnalysis.*

  given stateArbitrary:Arbitrary[List[State]] =
    Arbitrary(Gen.listOfN(15, Gen.oneOf(State.values)).map(x => List(x*)))

  val net = PPn.net

  property("test always ends in DONE or OP_A") = forAll: (mSet: List[State]) =>
    val res = net.completePathsUpToDepth(marking(mSet*), 100)
    res.forall(x => x.last.forall((_,tokens) =>
      tokens.forall(y => y == State.DONE || y == State.OP_A)))
