package scala.u06.examples


import scala.u06.lab.PetriNetFacade.PN
import scala.u06.modelling.SystemAnalysis

object Pn:
  import PN.*
  import SystemAnalysis.*
  enum Place:
    case N, T, C
  import Place.*


  def mutualExclusion = PN[Place](
    >(N) ~~> >(T),
    >(T) ~~> >(C) ^^^ >(C),
    >(C) ~~> >()
  )

    


      // string can also be used as places
   def rW = PN[String](
      >("p1") ~~> >("p2"),
      >("p2") ~~> >("p3"),
      >("p3","p5") ~~> >("p5","p6"),
      >("p2") ~~> >("p4"),
      >("p4, p5") ~~> >("p7") ^^^ >("p6"),
      >("p6") ~~> >("p1"),
      >("p7") ~~> >("p1"))

