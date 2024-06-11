package scala.u09.lab

import  scala.u09.lab.QMatrix.*

object Utils {

  def samePosition[S <: Pos2d[S]](s1:S,s2:S): Boolean =
    s1.x == s2.x && s1.y == s2.y
  def isPresent[S <: Pos2d[S]](p:(Int,Int), set: Set[S]): Boolean =
    set.exists(s2 => s2.x == p._1 && s2.y == p._2)

  extension[S](v:Pos2d[S])
    def pos: (Int, Int) = (v.x, v.y)
}




