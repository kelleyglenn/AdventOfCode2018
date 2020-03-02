package day21

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class ComputerTest extends AnyFlatSpec {
  behavior of "execute"
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    // By examining the input, I've been able to determine that the program exits at line 28-29 when Reg(2) == Reg(0), which is the input
    var first2: Long = 0

    def captureFirstResult(r: Registers): Boolean = {
      first2 = r._2
      if (r._4 == 27) println(r)
      r._4 == 28
    }

    var prevR: Option[Registers] = None

    def logBeforeAndAfter(r: Registers): Boolean = {
      if (prevR.isDefined) {
        println("" + (prevR.get._4 + 1) + ": " + prevR.get + "  =>  " + r)
      }
      prevR = Some(r)
      false
    }

    var c = new Computer(Registers(-1, 0, 0, 0, 0, 0))
    c.execute(Computer.stringsToInstructions(lines), captureFirstResult)
    c = new Computer(Registers(first2, 0, 0, 0, 0, 0))
    assert(c.execute(Computer.stringsToInstructions(lines), logBeforeAndAfter)._2 == 1848)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    var first2: Long = -1
    var cur2: Long = -1
    var prev2: Long = -1

    def debugAfter(r: Registers): Boolean = {
      if (r._4 == 28) {
        if (first2 < 0) first2 = r._2
        prev2 = cur2
        cur2 = r._2
        if (cur2 == first2 && prev2 != -1) {
          println(r)
          true
        } else false
      }
      else false
    }

    var c = new Computer(Registers(-1, 0, 0, 0, 0, 0))
    //c.execute(Computer.stringsToInstructions(lines), debugAfter)
    c = new Computer(Registers(prev2, 0, 0, 0, 0, 0))
    //assert(c.execute(Computer.stringsToInstructions(lines))._2 == 1848)
  }

  //  behavior of "SimulatedInput"
  //  it should "solve the first puzzle" in {
  //    var first2: Long = 0
  //    var debugCt = 0
  //
  //    def debugAfter(r: Registers): Boolean = {
  //      debugCt += 1
  //      first2 = r._2
  //      println(r)
  //      r._4 == 28 || debugCt > 500
  //    }
  //
  //    var sim = new SimulatedInput(Registers(-2, 0, 0, 0, 0, 0))
  //    sim.execute(debugAfter)
  //    sim = new SimulatedInput(Registers(first2, 0, 0, 0, 0, 0))
  //    sim.execute()
  //  }

  behavior of "SimulatedInput2"
  it should "solve the first puzzle" in {
    var first2: Long = 0
    var debugCt = 0

    def debugAfter(r: Registers): Boolean = {
      debugCt += 1
      first2 = r._2
      println(r)
      r._4 == 27 || debugCt > 500
    }

    var sim = new SimulatedInput2(-2)
    sim.execute(debugAfter)
    sim = new SimulatedInput2(first2)
    sim.execute()
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