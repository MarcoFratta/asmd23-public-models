package scala.u09.lab.examples

import scala.u09.lab.*

object TryEnemyMatrix extends App:

  import scala.u09.lab.QMatrix.*
  import scala.u09.lab.BasicMatrix.*
  import BasicMatrix.given
  import Move.*

  class M(width: Int,
          height: Int,
          initial: Pos = (0, 0),
          terminal: PartialFunction[Pos, Boolean] = {case _ => false},
          override val reward: PartialFunction[(Pos, Move), Double],
          jumps: PartialFunction[((Int, Int), Move), (Int, Int)] = Map.empty,
          obstacles: Set[(Int, Int)] = Set(),
          enemies: Set[(Int, Int)] = Set(),
          params: LearningParams = LearningParams.default)
    extends BasicMatrix(width, height, initial, terminal, reward, params)
      with Jumps[Pos, Move](jumps)
      with Obstacles[Pos, Move](obstacles)
      with OutsideBoundPenalty[Pos, Move](width, height)
      with Enemies[Pos, Move](enemies)


  @main
  def example(): Unit =
    val rl = M(7, 7,
      reward = {case _ => 0},
      enemies = Set((3, 3)))

    val q0 = rl.qFunction
    val q1 = rl.makeLearningInstance().learn(100000, 500, q0)
    println(rl.show(s => "%2.0f".format(q0.vFunction(s))))
    println(rl.show(s => "%2.0f".format(q1.vFunction(s))))
    println(rl.show(s => q1.bestPolicy(s).toString))

