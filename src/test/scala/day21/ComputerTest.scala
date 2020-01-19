package day21

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class ComputerTest extends AnyFlatSpec {
  behavior of "execute"
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    // By examining the input, I've been able to determine that the program exits at line 28-29 when Reg(2) == Reg(0), which is the input
    val c = new Computer(Registers(13522479, 0, 0, 0, 0, 0))
    val debug: Registers => Unit = (r: Registers) => {
      if (r._4 == 28) {
        println(r)
      }
    }
    assert(c.execute(Computer.stringsToInstructions(lines), debug)._2 == 1848)
  }

  behavior of "instructionsToMkStrings"
  it should "make the input more readable" in new SetupPuzzleData("input") {
    Computer.instructionsToMkStrings(Computer.stringsToInstructions(lines)).zipWithIndex.foreach { si: (String, Int) =>
      print((si._2 - 1).toString + (if (si._2 > 0 && si._2 < 11) "  " else " ")); println(si._1)
    }
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day21/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq.takeWhile(_.nonEmpty)
  }

}