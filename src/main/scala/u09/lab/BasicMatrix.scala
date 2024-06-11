package scala.u09.lab

import scala.u09.lab.QMatrix.*
import Utils.*
object BasicMatrix:

  enum Move:
    case LEFT, RIGHT, UP, DOWN

    override def toString = Map(LEFT -> "<", RIGHT -> ">",
      UP -> "^", DOWN -> "v")(this)

  case class Pos(override val x: Int, override val y: Int) extends Pos2d[Pos]:
    override def toString = s"($x,$y)"
    override def of(p: (Int, Int)): Pos = Pos(p._1, p._2)

  import Move.*
  given Conversion[(Int, Int), Pos] = p => Pos(p._1, p._2)

  case class BasicMatrix
  (width: Int,
   height: Int,
   override val initial: Pos,
   override val terminal: PartialFunction[Pos, Boolean],
   override val reward: PartialFunction[(Pos, Move), Double],
   override val params: LearningParams) extends Field[Pos, Move]:

    override type State = Pos
    override type Action = Move

    override def nextPos(p: (Int, Int), m: Move): (Int, Int) = m match
          case LEFT => (p._1 - 1, p._2)
          case RIGHT => (p._1 + 1, p._2)
          case UP => (p._1, p._2 - 1)
          case DOWN => (p._1, p._2 + 1)
    
    override def show[E](v: State => String): String =
      (for row <- 0 until height
           col <- 0 until width
      yield v(Pos(col,row)) +
        (if (col == width - 1) "\n" else "\t"))
        .mkString("")

    override def qFunction: Q = QFunction(Move.values.toSet, params.v0, terminal)