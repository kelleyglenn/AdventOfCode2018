package day16

import day16.OperationMatcher.{ADDI, Instruction, MULR, Registers, SETI, Sample}
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class OperationMatcherTest extends AnyFlatSpec {
  behavior of "operationsThatMatch"
  it should "handle the examples" in {
    assert(OperationMatcher.operationsThatMatch(Sample(Registers(3, 2, 1, 1), Instruction(9, 2, 1, 2), Registers(3, 2, 2, 1))) ==
      Set(MULR, ADDI, SETI))
  }

  behavior of "samplesThatMatchOperations"
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(OperationMatcher.samplesThatMatchOperations(OperationMatcher.stringsToSamples(lines.take(3124)), 3).size == 607)
  }

  behavior of "opCodesToOperations"
  it should "handle the puzzle" in new SetupPuzzleData("input") {
    assert(OperationMatcher.opCodesToOperations(OperationMatcher.stringsToSamples(lines.take(3124))).get.size == 16)
  }

  behavior of "runCode"
  it should "handle the puzzle" in new SetupPuzzleData("input") {
    val codedOperations: Map[Short, OperationMatcher.Operation] =
      OperationMatcher.opCodesToOperations(OperationMatcher.stringsToSamples(lines.take(3124))).get
    val instructions: Seq[Instruction] = OperationMatcher.stringsToInstructions(lines.drop(3126))
    assert(OperationMatcher.runCode(instructions, codedOperations, Registers(0, 0, 0, 0))._0 == 577)
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day16/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }

}
