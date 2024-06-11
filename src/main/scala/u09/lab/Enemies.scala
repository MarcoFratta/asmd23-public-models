package scala.u09.lab

import scala.u09.lab.QMatrix.*
import scala.u09.lab.Utils.*

trait Enemies[S <: Pos2d[S], A](n:Set[(Int,Int)]) extends Field[S, A]:

    override def show[E](v: S => String): String = super.show(s =>
        if n.contains(s.pos) then "E " else v(s))

    override def qEnvironment(): Environment = (s: S, a: A) =>
        val r:(Reward, State) = super.qEnvironment()(s, a)
        (s, a) match
            case (n,p) if isAnEnemy(nextPos(n.pos, p)) => (Double.MinValue, r._2)
            case (n,p) => (r._1 + totalDistanceFromEnemies(s.pos), r._2)

    private def totalDistanceFromEnemies(p: (Int, Int)): Double =
        n.map(p2 => distance(p, p2)).sum

    private def isAnEnemy(p: (Int, Int)): Boolean = n.contains(p)
    private def distance(p1: (Int, Int), p2: (Int, Int)): Double = {
        val dx = p1._1 - p2._1
        val dy = p1._2 - p2._2
        Math.sqrt(dx * dx + dy * dy)
    }

