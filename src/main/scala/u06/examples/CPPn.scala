package scala.u06.examples


import scala.u06.lab.PetriNetApi
import scala.u06.lab.PetriNetFacade.CPPN
import scala.u06.modelling

object CPPn:
  enum State:
    case P1, P2, P3
  enum Token:
    case A, B
  import CPPN.*
  import State.*
  import Token.*

  def net = CPPN[State](
    >(P1 <-- A, P1 <-- B) ~~> >(B --> P3) #-# 2,
    >(P1 <-- A) ~~> >(A --> P2) #-# 0,
    >(P2 <-- A) ~~> >(B --> P1) #-# 3,
    >(P3 <-- B) ~~> >() #-# 0
  ).toSystem



