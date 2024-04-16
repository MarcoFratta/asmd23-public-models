package scala.u06.examples


import scala.u06.utils.MSet
import scala.u06.modelling.PetriNet

object PNMutualExclusion:
  enum Place:
    case N, T, C
    
  export Place.*
  export scala.u06.modelling.PetriNet.*
  export scala.u06.modelling.SystemAnalysis.*
  export scala.u06.utils.MSet

  // DSL-like specification of a Petri Net
  def pnME = PetriNet[Place](
    MSet(N) ~~> MSet(T),
    MSet(T) ~~> MSet(C) ^^^ MSet(C),
    MSet(C) ~~> MSet()
  ).toSystem

@main def mainPNMutualExclusion =
  import PNMutualExclusion.*
  // example usage
  println(pnME.paths(MSet(N,N),7).toList.mkString("\n"))
