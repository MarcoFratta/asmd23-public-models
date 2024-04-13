package scala.u06

import scala.u06.lab.PetriNet.Marking
import scala.u06.lab.PetriNetFacade
import PetriNetFacade.{PPN}

object PriorityNetExample:

  enum State:
    case RES, OP_A, OP_B, DONE
  import PetriNetFacade.PPN.*
  import State.*
  def net = PPN[State](
    >(RES, RES) ~~> >(OP_A) #-# 1,
    (>(RES) ~~> >(OP_B) ^^^ >(OP_B)) #-# 2,
    >(RES, OP_A, OP_B) ~~> >(RES) #-# 3,
    >(OP_B) ~~> >(DONE) #-# 1
  ).toSystem