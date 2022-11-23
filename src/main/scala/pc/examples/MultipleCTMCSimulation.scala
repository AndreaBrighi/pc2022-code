package pc.examples

import pc.examples.StochasticChannel.State.IDLE
import pc.modelling.CTMCSimulation.*
import pc.utils.MSet
import pc.modelling.CTMC

import java.util.Random

object MultipleCTMCSimulation extends App :

  enum ResultsState:
    case SUCCESS(value: Double)
    case FAILURE(failed: Int)

  enum Results[T](val trace: Trace[T]):
    case SUCCESS(override val trace: Trace[T]) extends Results[T](trace)
    case TIMEOUT(override val trace: Trace[T]) extends Results[T](trace)


  type MultipleCTMCSimulationResult[T] = List[Results[T]]

  case class MultipleCTMCSimulation[T](ctmc: CTMC[T], initialState: T, run: Int):

    private def executeNRun(singoleSimulation: () => Results[T]): MultipleCTMCSimulationResult[T] =
      (0 until run)
        .toList
        .map(_ => singoleSimulation())

    def simulateUntil(p: Event[T] => Boolean, random: Random = new Random(), maxStep: Int = 500): MultipleCTMCSimulationResult[T] =
      executeNRun(() =>
        ctmc.newSimulationTrace(initialState, random)
          .take(maxStep)
          .foldLeft[Results[T]](Results.TIMEOUT[T](LazyList.empty)) { (acc, ev) =>
            (acc, ev) match
              case (Results.TIMEOUT(trace), event) if p(event) => Results.SUCCESS(trace :+ event)
              case (Results.SUCCESS(trace), _) => Results.SUCCESS(trace)
              case (Results.TIMEOUT(trace), event) => Results.TIMEOUT(trace :+ event)
          }
      )

    def simulate(steps: Int, random: Random = new Random()): MultipleCTMCSimulationResult[T] =
      executeNRun(() =>
        ctmc
          .newSimulationTrace(initialState, random)
          .take(steps) match
          case sim if sim.isEmpty => Results.TIMEOUT(LazyList.empty)
          case sim => Results.SUCCESS(sim)
      )

  extension[T] (results: List[Results[T]])
    def getAllSuccess = results.collect { case success: Results.SUCCESS[T] => success }

    def getAllTimeout = results.collect { case timeout: Results.TIMEOUT[T] => timeout }

  extension[T] (results: MultipleCTMCSimulationResult[T])

    def splitSuccessFromError = (results.getAllSuccess, results.getAllTimeout)

    def averageTimeOfComplete: Set[ResultsState] =
      val (success, timeout) = results.splitSuccessFromError
      val validSimulationsTime =
        success
          .map(_.trace)
          .map(trace => trace.lastOption.get.time)
      val failedSimulations = timeout.size
      (validSimulationsTime, failedSimulations) match
        case (Nil, 0) => Set.empty
        case (Nil, _) => Set(ResultsState.FAILURE(failedSimulations))
        case (times, 0) => Set(ResultsState.SUCCESS(times.sum / times.size))
        case (times, _) => Set(ResultsState.SUCCESS(times.sum / times.size), ResultsState.FAILURE(failedSimulations))

object StochasticChannelResult:

  import StochasticChannel.*
  import pc.examples.MultipleCTMCSimulation.MultipleCTMCSimulationResult
  import pc.examples.MultipleCTMCSimulation.ResultsState

  extension (results: MultipleCTMCSimulationResult[State])

    def averageTimeInError: Set[ResultsState] =
      val (success, timeout) = results.splitSuccessFromError
      val validSimulationsTrace =
        success
          .map(_.trace)
          .filter(trace => trace.lastOption.get.state == DONE)
      val failedSimulations = timeout.size + success.size - validSimulationsTrace.size
      val averageFailTime =
        validSimulationsTrace.map(
          _.foldLeft(
            (0.0, Event(0.0, IDLE))
          ) { (acc, event) =>
            acc match
              case (timeAcc, prevEvent) if prevEvent.state == FAIL => (timeAcc + event.time - prevEvent.time, event)
              case (timeAcc, _) if event.state == DONE => (timeAcc / event.time * 100, event)
              case (timeAcc, _) => (timeAcc, event)
          }._1)
      (averageFailTime, failedSimulations) match
        case (Nil, 0) => Set.empty
        case (Nil, _) => Set(ResultsState.FAILURE(failedSimulations))
        case (times, 0) => Set(ResultsState.SUCCESS(times.sum / times.size))
        case (times, _) => Set(ResultsState.SUCCESS(times.sum / times.size), ResultsState.FAILURE(failedSimulations))

import MultipleCTMCSimulation._
import StochasticChannelResult._

@main
def main(): Unit =
  import StochasticChannel.*
  val sim = MultipleCTMCSimulation.MultipleCTMCSimulation(stocChannel, IDLE, 6)
    .simulateUntil(_.state == DONE, maxStep = 6)
  println(sim.map(_.trace.toList.mkString(" ")).mkString("\n"))
  println(sim.averageTimeInError)
