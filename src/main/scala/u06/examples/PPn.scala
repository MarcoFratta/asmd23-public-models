package scala.u06.examples


import scala.u06.lab.PetriNetFacade.PPN
import scala.u06.modelling.SystemAnalysis



object PPn:
  enum State:
      case RES, OP_A, OP_B, DONE

  import PPN.*
  import State.*

  def net = PPN[State](
    >(RES, RES) ~~> >(OP_A) #-# 1,
    (>(RES) ~~> >(OP_B) ^^^ >(OP_B)) #-# 2,
    >(RES, OP_A, OP_B) ~~> >(RES) #-# 3,
    >(OP_B) ~~> >(DONE) #-# 1
  ).toSystem
  

  
