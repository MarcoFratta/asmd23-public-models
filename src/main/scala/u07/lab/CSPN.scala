package scala.u07.lab

import scala.u06.lab.PetriNetApi.{Box, Marking, Token, box}
import scala.u06.lab.PetriNetFacade.CPN
import scala.u07.modelling.CTMC
import scala.u07.modelling.CTMC.Action
import scala.u07.utils.MSet
import scala.util.Random

object CSPN:
  import SPN.*
  export SPN.{toSpn, toCTMC, **, rates}
  export CPN.*


  def apply[I](transitions: TrnWithRate[I, Token[?]]*): SPNImpl[I] = SPN(transitions*)

  def getRandomElement[A](m:Iterable[A], default: A):A =
    if m.isEmpty then default else m.toSeq.apply(Random.nextInt(m.size))

  extension [I](spn: SPNImpl[I])
    def CPNtoCTMC: CTMC[Marking[I, Token[?]]] = m =>
      val mSet = m.toList.flatMap((i, b) => List.fill(b.size)(i))
      val marking = MSet.ofList(mSet)
      for TrnWithRate(t, rate) <- spn.transitions.toSet
           if t.isEnabled(m)
           r = rate(marking)
           outs = t.fire(m)
           _ = println("possible outs " + outs.size)
           out <- outs
             yield Action(r / outs.size, out)
