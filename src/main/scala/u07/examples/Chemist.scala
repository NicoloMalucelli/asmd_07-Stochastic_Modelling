package scala.u07.examples

import u07.modelling.SPN.Trn
import u07.modelling.{CTMC, SPN}
import u07.utils.MSet

import java.util.Random
import scala.u07.examples.StochasticReadersAndWriters.Place.{SELECT, WAIT_FOR_READING, WAIT_FOR_WRITING}

object Chemist extends App:
  enum Place:
    case A, B, C, X, Y, D, E, R1, R2, R3, R4, R5, S1, S2, S3, S4, S5, SEQ

  export Place.*
  export u07.modelling.CTMCSimulation.*
  export u07.modelling.SPN.*

  /*
  val pnet1 = SPN[Place](
    Trn(MSet(A), m => 1.0, MSet(X), MSet()),
    Trn(MSet(X, X, Y), m => 1.0, MSet(X, X, X), MSet()),
    Trn(MSet(B, X), m => 1.0, MSet(Y, D), MSet()),
    Trn(MSet(X), m => 1.0, MSet(E), MSet()),
  )
   */

  val pnet2 = SPN[Place](
    Trn(MSet(A), m => 1.0, MSet(B), MSet()),
    Trn(MSet(A, A, B), m => 1.0, MSet(C), MSet()),
    Trn(MSet(C), m => 1.0, MSet(A, A, A), MSet()),
  )

  val pnet3 = SPN[Place](
    Trn(MSet(A, B, B), m => 1.0, MSet(C, D), MSet()),
    Trn(MSet(C), m => 1.0, MSet(A, C, C, C), MSet()),
    Trn(MSet(D), m => 1.0, MSet(B, B), MSet()),
  )

  val netInput = MSet.ofMap(Map((A, 10),(B, 17)))

  val k1 = 1;
  val k2 = 1;
  val k3 = 1;
  val k4 = 1;

  val pnet4 = SPN[Place](
    Trn(MSet(A), m => k1, MSet(X), MSet()),
    Trn(MSet(B, X), m => k3, MSet(Y, D), MSet()),
    Trn(MSet(X, X, Y), m => k2, MSet(X, X, X), MSet()),
    Trn(MSet(X), m => k4, MSet(C), MSet()),
/*
    Trn(MSet(R1), m => 1.0, MSet(SEQ), MSet()),
    Trn(MSet(R2), m => 1.0, MSet(SEQ), MSet()),
    Trn(MSet(R3), m => 1.0, MSet(SEQ), MSet()),
    Trn(MSet(R4), m => 1.0, MSet(SEQ), MSet()),
    Trn(MSet(R5), m => 1.0, MSet(SEQ), MSet()),
    Trn(MSet(SEQ, S1), m => 1.0, MSet(R1, S2) union netInput, MSet()),
    Trn(MSet(SEQ, S2), m => 1.0, MSet(R2, S3) union netInput, MSet()),
    Trn(MSet(SEQ, S3), m => 1.0, MSet(R3, S4) union netInput, MSet()),
    Trn(MSet(SEQ, S4), m => 1.0, MSet(R4, S5) union netInput, MSet()),
    Trn(MSet(SEQ, S5), m => 1.0, MSet(R5, S1) union netInput, MSet()),
 */
  )

  import u07.modelling.CTMCSimulation.*
  import u07.modelling.CTMCSimulation.SimulationAnalyzer.meanRelativeTimeWhile
  import u07.modelling.CTMCSimulation.plot

  val numOfA = 10000000
  val numOfB = 17000000

  val simulation = toCTMC(pnet4).simulateOnce(MSet.ofMap(Map((A, numOfA), (B, numOfB), (SEQ, 1), (S1, 1))), new Random)(1_000)

  println(
    simulation.plot(
      ("# of X", _.frequencyOf(X)),
      ("# of Y", _.frequencyOf(Y)),
      //("# of E", _.frequencyOf(E)),
      //("# of D", _.frequencyOf(D)),
      //("token", _.frequencyOf(TOKEN))
    )
  )