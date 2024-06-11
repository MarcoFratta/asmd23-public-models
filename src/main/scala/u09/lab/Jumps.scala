package scala.u09.lab

import scala.u09.lab.QMatrix.Pos2d
import scala.u09.lab.QMatrix.*
import Utils.*

trait Jumps[S <: Pos2d[S], A](jumps: PartialFunction[((Int,Int), A), (Int,Int)]) extends Field[S, A]:
    override def nextPos(s: (Int,Int), a: A): (Int,Int) =
      jumps.orElse(s2 => super.nextPos(s2._1, s2._2))(s, a)

    def printJumps[E](q: Q): S => String =
      s => if jumps.isDefinedAt((s.pos, q.bestPolicy(s)))
      then "J " else q.bestPolicy(s).toString
