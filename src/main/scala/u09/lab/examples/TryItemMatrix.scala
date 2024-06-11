package scala.u09.lab.examples

import scala.u09.lab.BasicMatrix.*
import scala.u09.lab.ItemsMatrix.{ItemsMatrix, PosWithItems}
import scala.u09.lab.Jumps
import scala.u09.lab.Obstacles
import scala.u09.lab.OutsideBoundPenalty
import scala.u09.lab.QMatrix.LearningParams


object TryItemMatrix extends App:

  import Move.*

  type State = PosWithItems

  class M(width: Int,
          height: Int,
          initial: State,
          terminal: PartialFunction[State, Boolean] = {case _ => false},
          override val reward: PartialFunction[(State, Move), Double] = {case _ => 0},
          jumps: PartialFunction[((Int,Int), Move), (Int,Int)] = Map.empty,
          releasePos: Set[(Int,Int)] = Set(),
          items: Set[(Int,Int)] = Set(),
          rewardPerItem: Double = 1,
          rewardPerRelease: Double = 10,
          obstacles: Set[(Int,Int)] = Set(),
          params: LearningParams = LearningParams.default)
    extends ItemsMatrix(width, height, initial, terminal,
    items, releasePos, rewardPerItem, rewardPerRelease, reward, params)
    with Obstacles[State, Move](obstacles)
    with Jumps[State, Move](jumps)
    with OutsideBoundPenalty[State, Move](width, height)


  @main
  def withObstaclesAndJumps(): Unit =
    val items = Set((4,4))
    val rl = M(5, 5,
      initial = (0,0,Set()),
      terminal = {case _ => false},
      reward = {case _ => 0},
      jumps = {
        case ((2, 0), RIGHT) => (4, 0)
      },
      obstacles = Set((3,2),(3,1),(3,0),(3,3),(4,3)),
      releasePos = Set((4,2)),
      items = items,
      rewardPerItem = 0,
      rewardPerRelease = 1)

    val q0 = rl.qFunction
    println(rl.show(s => "%2.1f".format(q0.vFunction(s))))
    val q1 = rl.makeLearningInstance().learn(10000, 100, q0)
    println(rl.show(s => "%2.1f".format(q1.vFunction(s))))
    println(rl.show(rl.printJumps(q1)))

  @main
  def multipleItems():Unit =
    val params = LearningParams(0.9, 0.5, 0.45, 0)
    val items = Set((5,0), (0,5))
    val rl = M(6, 6,
      initial = (0, 0, Set()),
      terminal = {case _ => false},
      releasePos = Set((5, 5)),
      items = items,
      rewardPerItem = 0,
      rewardPerRelease = 1)

    val q0 = rl.qFunction
    println(rl.show(s => "%2.1f".format(q0.vFunction(s))))
    val q1 = rl.makeLearningInstance().learn(10000, 500, q0)
    println(rl.show(s => "%2.1f".format(q1.vFunction(s))))
    println(rl.show(s => q1.bestPolicy(s).toString))
