package day7

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class StepAnalyzerTest extends AnyFlatSpec {
  behavior of "stringsToSteps"
  it should "handle the example" in new SetupExampleData {
    assert(StepAnalyzer.stringsToSteps(strings) == steps)
  }

  behavior of "stepsToOrder"
  it should "handle the example" in new SetupExampleData {
    assert(StepAnalyzer.stepsToOrder(steps) == "CABDFE")
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(StepAnalyzer.stepsToOrder(StepAnalyzer.stringsToSteps(lines)) == "EBICGKQOVMYZJAWRDPXFSUTNLH")
  }

  behavior of "stepsToOrder2"
  it should "solve the previous puzzle" in new SetupPuzzleData("input") {
    val steps: Set[(Char, Char)] = StepAnalyzer.stringsToSteps(lines)
    assert(StepAnalyzer.stepsToOrder2(steps, 1, (_: Char) => { 0 })._1 == "EBICGKQOVMYZJAWRDPXFSUTNLH")
  }
  it should "handle the example" in new SetupExampleData {
    assert(StepAnalyzer.stepsToOrder2(steps, 2, _ - 'A' + 1) == ("CABFDE", 15))
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    val steps: Set[(Char, Char)] = StepAnalyzer.stringsToSteps(lines)
    assert(StepAnalyzer.stepsToOrder2(steps, 5, _ - 'A' + 61) == ("EIVZBCGYJKAQWORMDPXFSUTNLH", 906))
  }

  class SetupExampleData {

    val steps = Set(
      ('C', 'A'),
      ('C', 'F'),
      ('A', 'B'),
      ('A', 'D'),
      ('B', 'E'),
      ('D', 'E'),
      ('F', 'E')
    )

    val strings: Seq[String] = Seq(
      "Step C must be finished before step A can begin.",
      "Step C must be finished before step F can begin.",
      "Step A must be finished before step B can begin.",
      "Step A must be finished before step D can begin.",
      "Step B must be finished before step E can begin.",
      "Step D must be finished before step E can begin.",
      "Step F must be finished before step E can begin."
    )
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day7/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }

}
