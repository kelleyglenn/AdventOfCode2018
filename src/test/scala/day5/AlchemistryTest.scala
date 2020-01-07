package day5

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class AlchemistryTest extends AnyFlatSpec {
  behavior of "reduce"
  it should "handle the examples" in {
    assert(Alchemistry.reduce("aA") == "")
    assert(Alchemistry.reduce("abBA") == "")
    assert(Alchemistry.reduce("abAB") == "abAB")
    assert(Alchemistry.reduce("aabAAB") == "aabAAB")
    assert(Alchemistry.reduce("dabAcCaCBAcCcaDA") == "dabCBAcaDA")
    assert(Alchemistry.reduce("dabAcCaCBAcCcaDA").length == 10)
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(Alchemistry.reduce(lines.head).length == 11668)
  }

  behavior of "findSmallestReductionRemovingOneProblem"
  it should "handle the examples" in {
    assert(Alchemistry.findSmallestReductionRemovingOneProblem("dabAcCaCBAcCcaDA") == ('c', "daDA"))
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(Alchemistry.findSmallestReductionRemovingOneProblem(lines.head)._2.length == 4652)
  }

  class SetupPuzzleData(name: String) {

    val bufferedSource: BufferedSource =
      Source.fromURL(getClass.getResource("/day5/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }
}
