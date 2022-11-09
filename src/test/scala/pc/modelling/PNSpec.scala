package pc.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import pc.modelling.PetriNet.Marking

class PNSpec extends AnyFunSuite {

  import pc.examples.PNMutualExclusion.*

  test("PN for mutual exclusion should properly generate 7-length paths") {

    val expected1 = List(MSet(N, N), MSet(T, N), MSet(T, T), MSet(C, T), MSet(T), MSet(C), MSet())
    val expected2 = List(MSet(N, N), MSet(T, N), MSet(C, N), MSet(C, T), MSet(T), MSet(C), MSet())
    val expected3 = List(MSet(N, N), MSet(T, N), MSet(C, N), MSet(N), MSet(T), MSet(C), MSet())

    pnME.paths(MSet(N, N), 7).toSet shouldBe Set(expected1, expected2, expected3)
  }

  test("PN for mutual exclusion should properly generate 0-length paths") {
    pnME.paths(MSet(N, N), 0).toSet shouldBe Set.empty
  }

  test("PN for mutual exclusion should properly generate 1-length paths") {
    val expected = List(MSet(N, N))
    pnME.paths(MSet(N, N), 1).toSet shouldBe Set(expected)
  }

  test("PN for mutual exclusion should properly generate the possible transitions in the mutual exclusion") {
    val expected = MSet(C, T, T)
    pnME.next(MSet(T, T, T)) shouldBe Set(expected)
  }

  test("PN for mutual exclusion should properly generate the possible transitions in the mutual exclusion release") {
    val expected = MSet( T, T)
    pnME.next(MSet(C, T, T)) shouldBe Set(expected)
  }

  test("PN for mutual exclusion should properly generate the possible transitions in the mutual exclusion request") {
    val expected = MSet(T, C)
    pnME.next(MSet(T, T)) shouldBe Set(expected)
  }

  test("PN for mutual exclusion should properly generate the possible transitions in the mutual exclusion multiple release") {
    val expected = List(MSet(C, T, T), MSet(T, T), MSet(C, T), MSet(T), MSet(C), MSet())
    pnME.paths(MSet(C, T, T), 6).toSet shouldBe Set(expected)
  }
  
}
