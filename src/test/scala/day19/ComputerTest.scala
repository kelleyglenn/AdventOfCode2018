package day19

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class ComputerTest extends AnyFlatSpec {
  behavior of "stringsToInstructions"
  it should "handle the example" in new SetupExampleData {
    assert(Computer.stringsToInstructions(instructions).head == IPInstruction(0))
  }

  behavior of "execute"
  it should "handle the first two lines of the example" in new SetupExampleData {
    val c = new Computer(Registers(0, 0, 0, 0, 0, 0))
    assert(c.execute(Computer.stringsToInstructions(instructions.take(2))) == Registers(0, 5, 0, 0, 0, 0))
    assert(c.ip == 1)
  }
  it should "handle the first three lines of the example" in new SetupExampleData {
    val c = new Computer(Registers(0, 0, 0, 0, 0, 0))
    assert(c.execute(Computer.stringsToInstructions(instructions.take(3))) == Registers(1, 5, 6, 0, 0, 0))
    assert(c.ip == 2)
  }
  it should "handle the first four lines of the example" in new SetupExampleData {
    val c = new Computer(Registers(0, 0, 0, 0, 0, 0))
    assert(c.execute(Computer.stringsToInstructions(instructions.take(4))) == Registers(3, 5, 6, 0, 0, 0))
    assert(c.ip == 4)
  }
  it should "handle the example" in new SetupExampleData {
    val c = new Computer(Registers(0, 0, 0, 0, 0, 0))
    assert(c.execute(Computer.stringsToInstructions(instructions))._0 == 6)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    val c = new Computer(Registers(0, 0, 0, 0, 0, 0))
    val debug: Registers => Unit = (r: Registers) => {
      println(r)
    }
    assert(c.execute(Computer.stringsToInstructions(lines), debug)._0 == 2821)
  }

  // By examining the state of the registers when Reg0 changes, I was able to determine what the program is trying to do
  behavior of "manual analysis of registers while program is running"
  it should "handle the first puzzle" in {
    val factorsOf900 = Set(1, 2, 3, 4, 5, 6, 9, 10, 12, 15, 18, 20, 25, 30, 36, 45, 50, 60, 75, 90, 100, 150, 180, 225, 300, 450, 900)
    assert(factorsOf900.sum == 2821)
  }
  it should "solve the second puzzle" in {
    val factorsOf10551300 = Set(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 25, 30, 50, 60, 75, 100, 150, 300, 35171, 70342, 105513, 140684, 175855, 211026, 351710, 422052, 527565, 703420, 879275, 1055130, 1758550, 2110260, 2637825, 3517100, 5275650, 10551300)
    assert(factorsOf10551300.sum == 30529296)
  }

  class SetupExampleData {
    val instructions = Seq(
      "#ip 0",
      "seti 5 0 1",
      "seti 6 0 2",
      "addi 0 1 0",
      "addr 1 2 3",
      "setr 1 0 0",
      "seti 8 0 4",
      "seti 9 0 5"
    )
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day19/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }

}
