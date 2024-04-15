package u07.modelling

import breeze.linalg.*
import breeze.plot.*
import u07.modelling.CTMCSimulation.SimulationResult

import java.util.Random
import u07.utils.Stochastics

import java.awt.Color

object CTMCSimulation:

  case class Event[A](time: Double, state: A)
  type Trace[A] = LazyList[Event[A]]
  type SimulationResult[A] = List[Event[A]]

  export CTMC.*

  import scala.u07.utils.List.*

  extension [S](self: CTMC[S])
    def newSimulationTrace(s0: S, rnd: Random): Trace[S] =
      LazyList.iterate(Event(0.0, s0)):
        case Event(t, s) =>
          if self.transitions(s).isEmpty
          then
            Event(t, s)
          else
            val choices = self.transitions(s) map (t => (t.rate, t.state))
            val next = Stochastics.cumulative(choices.toList)
            val sumR = next.last._1
            val choice = Stochastics.draw(next)(using rnd)
            Event(t + Math.log(1 / rnd.nextDouble()) / sumR, choice)
    def simulateOnce(s0: S, rnd: Random)(stopCondition: Event[S] => Boolean): SimulationResult[S] =
      newSimulationTrace(s0, rnd).takeUntil(!stopCondition(_)).toList

    def simulateOnce(s0: S, rnd: Random)(depth: Int): SimulationResult[S] =
      newSimulationTrace(s0, rnd).take(depth).toList

    def simulateNTimes(s0: S, rnd: Random)(stopCondition: Event[S] => Boolean)(n: Int) : List[SimulationResult[S]] =
      Range(0, n).map(_ => simulateOnce(s0, rnd)(stopCondition)).toList

    def simulateNTimes(s0: S, rnd: Random)(depth: Int)(n: Int): List[SimulationResult[S]] =
      Range(0, n).map(_ => simulateOnce(s0, rnd)(depth)).toList

  extension [S](self: SimulationResult[S])
    def plot(variables: (String, (S => Double))*): Unit =
      val x = self.map(_.time)
      val fig = Figure()

      val plt = fig.subplot(0)
      plt.legend = true
      plt.xlabel = "time units"
      plt.ylabel = ""

      val ys = variables
        .map(variable => (variable._1, self.map(ev => variable._2.apply(ev.state))))
        .foreach((name, l) =>

          plt += breeze.plot.plot(x, DenseVector(l.toArray), name=name)
        )
      fig.refresh()


  object SimulationAnalyzer:
    import scala.u07.utils.List.*

    extension [S](self: List[SimulationResult[S]])
      def meanTimeToReachS(s: S): Option[Double] =
        self.map(l => l.find(e => e.state.equals(s))) match
          case x if x.contains(Option.empty) => Option.empty //if one of the runs doesn't have the s state, return empty option
          case x => Some(x.map(e => e.get.time).mean)

      def meanRelativeTimeWhileInState(s: S): Double =
        meanRelativeTimeWhile(_ == s)

      def ratioOfSimulationSatisfying(cond: SimulationResult[S] => Boolean): Double =
        self.count(cond).toDouble / self.size


      def meanRelativeTimeWhile(f: S => Boolean): Double =
        self.map(l =>
          l.zip(l.slice(0, l.length - 1).prepended(Event(0, null)))
            .filter(l => f.apply(l._1.state))
            .map(l => l._1.time - l._2.time)
            .sum
          /
          l.last.time
        ).mean


//def analyze[A](map: Event[S] => A)(fold: (A, A) => A): A = ???

import u07.modelling.CTMCSimulation.simulateNTimes
import u07.examples.StochasticChannel.State.*
import u07.examples.StochasticChannel.stocChannel
import u07.modelling.CTMCSimulation.SimulationAnalyzer.*
@main def main() =

  val simulation = stocChannel.simulateNTimes(IDLE, new Random)(_.state == DONE)(100)

  println(
    simulation.meanTimeToReachS(DONE) match
      case None => "one or more simulation's run do not have that state"
      case Some(x) => x.toString + " time units to reach state DONE"
  )

  println(
    simulation.meanRelativeTimeWhileInState(FAIL) +
    "% of time passed in FAIL state"
  )

  println(
    simulation.ratioOfSimulationSatisfying(l => l.count(_.state == FAIL) == 0) +
      "% of simulations didn't go in FAIL state"
  )