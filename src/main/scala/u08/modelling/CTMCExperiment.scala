package scala.u08.modelling

import java.util.Random
import scala.u07.modelling.CTMC

object CTMCExperiment:

  import CTMCSimulation.*

  opaque type Property[A] = Trace[A] => Boolean

  given rnd: Random = new Random

  extension [S](self: CTMC[S])
    // globally is simply achieved by equivalence not G x= F not x
    def eventually[A](filt: A => Boolean): Property[A] =
      trace => trace exists (e => filt(e.state))

    def always[A](filt: A => Boolean): Property[A] =
      trace => trace forall (e => filt(e.state))

    // takes a property and makes it time bounded by the magics of streams
    def bounded[A](timeBound: Double)(prop: Property[A]): Property[A] =
      trace => prop(trace takeWhile (_.time <= timeBound))

    // a PRISM-like experiment, giving a statistical result (in [0,1])
    def experiment(runs: Int, prop: Property[S], s0: S, timeBound: Double): Double =
      (0 until runs).count: _ =>
        bounded(timeBound)(prop)(self.newSimulationTrace(s0 ,rnd))
      .toDouble/runs
    def simulation[K](range: Seq[BigDecimal], s0: S, f: Trace[S] => K): Seq[(Double,K)] =
      for
        t <- range
        trace = self.newSimulationTrace(s0, rnd) takeWhile (_.time <= t)
      yield t.doubleValue -> f(trace)
    def analyze[K](bound:Double, s0: S, f: S => K): Seq[(Double,K)] =
      for s <- self.newSimulationTrace(s0, rnd) takeWhile (_.time <= bound)
          _ = println(s)
          yield s.time -> f(s.state)

