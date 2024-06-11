package scala.u09.lab

import scala.u09.lab.QMatrix.Pos2d
import scala.u09.lab.QMatrix.*
import scala.u09.lab.Utils.*

trait Obstacles[S <: Pos2d[S], A](obstacles: Set[(Int,Int)]) extends Field[S, A]:
    override def show[E](v: S => String): String =
      super.show(s => if isAnObstacle(s.pos) then "X " else v(s))

    override def qEnvironment(): Environment = (s: S, a: A) => (s, a) match
        case (n, m) if isAnObstacle(nextPos(n.pos, m)) => (Double.MinValue, s)
        case _ => super.qEnvironment()(s, a)

    private def isAnObstacle(s: (Int, Int)): Boolean = obstacles.contains(s)
