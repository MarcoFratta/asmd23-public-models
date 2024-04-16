package scala.u07.lab

import scala.u07.modelling.CTMC
import scala.u07.modelling.CTMC.Action

import scala.u06.lab.PetriNetApi.Transition.TransitionImpl
import scala.u06.lab.PetriNetApi.{PetriNet, Token, Transition}
import scala.u06.modelling.PetriNet.Marking
import scala.u07.utils.MSet

object SPN:
  export scala.u06.lab.PetriNetFacade.PN.*

  case class TrnWithRate[I,P](trn: Transition[I,P], priority: MSet[I] => Double)
    extends TransitionImpl[I,P](trn.condition, trn.action, trn.name)

  extension [I,P](t: Transition[I,P])
    def *(rate: Double): TrnWithRate[I,P] =
      TrnWithRate(t, _ => rate)
    def * (rate: MSet[I] => Double): TrnWithRate[I, P] =
        TrnWithRate(t, rate)
  extension [I](p:PetriNet[I])
    def toSpn(rates:(MSet[I] => Double)*): SPNImpl[I] = if rates.size != p.transitions.size
      then throw IllegalArgumentException("Wrong number of rates")
      else
        val it = rates.iterator
        SPNImpl(p.transitions.map(TrnWithRate(_,it.next())).toSeq*)
  case class SPNImpl[I](transitions: TrnWithRate[I, Token[?]]*)
  def apply[I](transitions: TrnWithRate[I, Token[?]]*): SPNImpl[I] = SPNImpl(transitions*)

  import scala.u06.lab.PetriNetApi.*
  import scala.u06.lab.PetriNetApi.Marking
  extension [I](spn: SPNImpl[I])
    def toCTMC: CTMC[Marking[I, Token[?]]] = m =>
          val mSet = m.toList.flatMap((i,b) => List.fill(b.size)(i))
          print(s"mSet: $mSet")
          val marking = MSet.ofList(mSet)
          for TrnWithRate(t, rate) <- spn.transitions.toSet
              if t.isEnabled(m)
              r = rate(marking)
          yield Action(r, t.fire(m))
