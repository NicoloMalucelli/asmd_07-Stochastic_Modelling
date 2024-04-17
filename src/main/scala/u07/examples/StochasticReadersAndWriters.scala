package scala.u07.examples

import u07.modelling.SPN.Trn
import u07.modelling.{CTMC, SPN}
import u07.utils.MSet

import java.util.Random
import scala.u07.examples.StochasticReadersAndWriters.Place.{SELECT, WAIT_FOR_READING, WAIT_FOR_WRITING}

object StochasticReadersAndWriters extends App:
  enum Place:
    case IDLE, SELECT, WAIT_FOR_READING, WAIT_FOR_WRITING, TOKEN, READING, WRITING, WANT_TO_WRITE, WANT_TO_READ

  export Place.*
  export u07.modelling.CTMCSimulation.*
  export u07.modelling.SPN.*

  val rw = SPN[Place](
    Trn(MSet(IDLE), m => 1.0,   MSet(SELECT),  MSet()),
    Trn(MSet(SELECT), m => 200_000,  MSet(WAIT_FOR_READING),  MSet()),
    Trn(MSet(SELECT), m => 100_000,   MSet(WAIT_FOR_WRITING),   MSet()),
    Trn(MSet(WAIT_FOR_READING, TOKEN), m => 100_000,  MSet(READING, TOKEN),  MSet()),
    Trn(MSet(WAIT_FOR_WRITING, TOKEN), m => 100_000,   MSet(WRITING), MSet(READING)),
    Trn(MSet(READING), m => 0.1 * m(READING),  MSet(IDLE),  MSet()),
    Trn(MSet(WRITING), m => 0.1,   MSet(IDLE, TOKEN),   MSet())
  )

  val instantly = 1_000_000
  val rw2 = SPN[Place](
    Trn(MSet(IDLE), m => 0.0001, MSet(SELECT), MSet()),

    Trn(MSet(SELECT, TOKEN), m => 0.00011, MSet(WAIT_FOR_READING, TOKEN), MSet()),
    Trn(MSet(SELECT, TOKEN), m => 0.00011, MSet(WAIT_FOR_WRITING), MSet()),

    Trn(MSet(WAIT_FOR_READING), m => 0.00011,  MSet(READING),  MSet(WRITING)),
    Trn(MSet(WAIT_FOR_WRITING), m => 0.00011, MSet(WRITING), MSet(WRITING, READING)),

    Trn(MSet(READING), m => 0.00011, MSet(IDLE), MSet()),
    Trn(MSet(WRITING), m => 0.0001, MSet(IDLE, TOKEN), MSet())
  )

  import u07.modelling.CTMCSimulation.*
  import u07.modelling.CTMCSimulation.SimulationAnalyzer.meanRelativeTimeWhile
  import u07.modelling.CTMCSimulation.plot

  val k = 10
  /*
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
*/
  val sim = toCTMC(rw2).simulateOnce(MSet.ofMap(Map((IDLE, k), (TOKEN, 1))), new Random)(300)
  print(sim.size)
  println(
    List(sim).meanRelativeTimeWhile(s => s.frequencyOf(WRITING) > 0)
  )
  println(
    List(sim).meanRelativeTimeWhile(s => s.frequencyOf(READING) > 0)
  )
  println(
    sim.plot(
      ("# of readers", _.frequencyOf(READING)),
      ("# of writers", _.frequencyOf(WRITING)),
      //("token", _.frequencyOf(TOKEN))
    )
  )