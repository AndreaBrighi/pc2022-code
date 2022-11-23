package pc.examples

import pc.modelling.{CTMC, SPN}
import pc.utils.MSet
import java.util.Random

object ReaderAndWriter:
  enum Place:
    case P1, P2, P3, P4, P5, P6, P7;

  export Place.*
  export pc.modelling.CTMCSimulation.*
  export pc.modelling.SPN.*


  val spn = SPN[Place](
    Trn(MSet(P1), m => m(P1)*1_000, MSet(P2), MSet()),
    Trn(MSet(P2), m => 100_000.0, MSet(P3), MSet()),
    Trn(MSet(P2), m => 100_000.0, MSet(P4), MSet()),
    Trn(MSet(P3, P5), m => 1.0, MSet(P6, P5), MSet()),
    Trn(MSet(P6), m => 1_000.0, MSet(P1), MSet()),
    Trn(MSet(P4, P5), m => 1_000.0, MSet(P7), MSet(P6)),
    Trn(MSet(P7), m => 1.0, MSet(P5, P1), MSet())
  )


@main def mainStochasticReaderAndWriter() = // example run
  import ReaderAndWriter.*
  println(
    toCTMC(spn)
      .newSimulationTrace(MSet(P1, P1, P1, P5), new Random)
      .take(20)
      .toList
      .mkString("\n"))
