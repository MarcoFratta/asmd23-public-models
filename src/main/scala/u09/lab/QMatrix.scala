package scala.u09.lab

import scala.u09.model.QRLImpl
import Utils.*

object QMatrix:

  trait Pos2d[S]:
    def x: Int
    def y: Int
    def of(p: (Int,Int)): S
  trait LearningParams:
    val gamma: Double
    val alpha: Double
    val epsilon: Double
    val v0: Double
  object LearningParams:
    def apply(g: Double, a: Double, e: Double, v: Double): LearningParams =
      new LearningParams {
        val gamma = g
        val alpha = a
        val epsilon = e
        val v0 = v
      }
    def default: LearningParams = apply(0.9, 0.5, 0.3, 1.0)


  trait Field[S <: Pos2d[S], A] extends QRLImpl:
    val initial: S
    val terminal: PartialFunction[S, Boolean]
    val reward: PartialFunction[(S, A), Double]
    val params: LearningParams


    override type State = S
    override type Action = A
    def qEnvironment(): Environment = (s: S, a: A) => (reward.apply((s, a)), nextState(s, a))

    protected def nextState(s: S, a: A): S = ->(s,a)

    protected def nextPos(p: (Int,Int), m: A): (Int,Int) = p
    protected def ->(s:S, a:A):S  = s.of(nextPos(s.pos, a))
    def show[E](v: State => String): String = "Not implemented"
    def qFunction: Q
    def qSystem = QSystem(environment = qEnvironment(), initial, terminal)
    def makeLearningInstance() =
      QLearning(qSystem, params.gamma,
        params.alpha, params.epsilon, qFunction)