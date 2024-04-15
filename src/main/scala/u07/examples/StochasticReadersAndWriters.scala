package scala.u07.examples

import u07.modelling.SPN.Trn
import u07.modelling.{CTMC, SPN}
import u07.utils.MSet

import java.util.Random
import scala.u07.examples.StochasticReadersAndWriters.Place.{SELECT, WAIT_FOR_READING, WAIT_FOR_WRITING}

object StochasticReadersAndWriters extends App:
  enum Place:
    case IDLE, SELECT, WAIT_FOR_READING, WAIT_FOR_WRITING, TOKEN, READING, WRITING

  export Place.*
  export u07.modelling.CTMCSimulation.*
  export u07.modelling.SPN.*

  val rw = SPN[Place](
    Trn(MSet(IDLE), m => 1.0,   MSet(SELECT),  MSet()),
    Trn(MSet(SELECT), m => 200_000,  MSet(WAIT_FOR_READING),  MSet()),
    Trn(MSet(SELECT), m => 100_000,   MSet(WAIT_FOR_WRITING),   MSet()),
    Trn(MSet(WAIT_FOR_READING, TOKEN), m => 100_000,  MSet(READING, TOKEN),  MSet()),
    Trn(MSet(WAIT_FOR_WRITING, TOKEN), m => 100_000,   MSet(WRITING),   MSet(READING)),
    Trn(MSet(READING), m => 0.1 * m(READING),  MSet(IDLE),  MSet()),
    Trn(MSet(WRITING), m => 0.1,   MSet(IDLE, TOKEN),   MSet())
  )

  import u07.modelling.CTMCSimulation.*
  import u07.modelling.CTMCSimulation.SimulationAnalyzer.meanRelativeTimeWhile
  import u07.modelling.CTMCSimulation.plot

  val k = 5
  val simulation = toCTMC(rw).simulateNTimes(MSet.ofMap(Map((IDLE, k), (TOKEN, 1))), new Random)(50)(20)
  println(
    simulation.meanRelativeTimeWhile(s => s.frequencyOf(WRITING) > 0) +
    "% of time someone was writing"
  )

  println(
    simulation.meanRelativeTimeWhile(s => s.frequencyOf(READING) > 0) +
      "% of time someone was reading"
  )

  println(
    simulation.meanRelativeTimeWhile(s => s.frequencyOf(WRITING) == 0 && s.frequencyOf(READING) == 0) +
      "% nobody was writing or reading"
  )

  println(
    toCTMC(rw).simulateOnce(MSet.ofMap(Map((IDLE, k), (TOKEN, 1))), new Random)(50).plot(
      ("# of readers", _.frequencyOf(READING)),
      ("# of writers", _.frequencyOf(WRITING)),
      //("token", _.frequencyOf(TOKEN))
    )
  )