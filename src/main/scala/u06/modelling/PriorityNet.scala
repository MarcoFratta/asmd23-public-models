package scala.u06.modelling

import scala.u06.modelling.PetriNet.Trn
import scala.u06.utils.MSet


object PriorityNet:

  case class TrnWithPriority[P](trn: Trn[P], priority: Int)
  type PetriNet[P] = Set[TrnWithPriority[P]]
  type Marking[P] = MSet[P]

  def apply[P](trnWithPriority: TrnWithPriority[P]*): PetriNet[P] =
    trnWithPriority.toSet

  extension [P](pn: PetriNet[P])
    def toSystem: System[Marking[P]] = m =>
      val firables = pn.filter(t => m.disjoined(t.trn.inh) && m.matches(t.trn.cond))
      val max = firables.map(_.priority).maxOption((a,b) => a - b).getOrElse(0)
      for
        TrnWithPriority(trn, priority) <- firables filter(_.priority == max)
        out <- m extract trn.cond
      yield out union trn.eff



  extension[P](trn:Trn[P])
   def <>(priority:Int):TrnWithPriority[P] = TrnWithPriority(trn,priority)





