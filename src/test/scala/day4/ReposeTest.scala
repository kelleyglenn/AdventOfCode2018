package day4

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class ReposeTest extends AnyFlatSpec {
  behavior of "stringsToRecords"
  it should "handle a simple example" in {
    val lines = Seq(
      "[1518-11-01 00:25] wakes up",
      "[1518-11-01 00:00] Guard #10 begins shift",
      "[1518-11-01 00:05] falls asleep"
    )
    val expected = Map(
      "10" -> Seq(5 until 25)
    )
    assert(Repose.stringsToRecords(lines) == expected)
  }
  it should "handle the full example" in new SetupExampleData {

    val expected = Map(
      "10" -> Seq(5 until 25, 30 until 55, 24 until 29),
      "99" -> Seq(40 until 50, 36 until 46, 45 until 55)
    )
    assert(Repose.stringsToRecords(lines) == expected)
  }

  behavior of "recordsToMinsSleeping"
  it should "handle the example" in new SetupExampleData {
    assert(Repose.recordsToMinsSleeping(Repose.stringsToRecords(lines)) == Set(("10", 50), ("99", 30)))
  }

  behavior of "idOfGuardSleepingMost"
  it should "handle the example" in new SetupExampleData {
    assert(Repose.idOfGuardSleepingMost(Repose.stringsToRecords(lines)) == "10")
  }

  behavior of "numberAppearingMostWithCount"
  it should "handle a simple example" in {
    assert(Repose.numberAppearingMostWithCount(Seq(1 to 25, 25 to 30)) == (25, 2))
  }
  it should "handle the example" in new SetupExampleData {
    assert(Repose.numberAppearingMostWithCount(Repose.stringsToRecords(lines)("10")) == (24, 2))
  }

  behavior of "Repose"
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(Repose.idOfGuardSleepingMost(Repose.stringsToRecords(lines)) == "3491")
    assert(Repose.numberAppearingMostWithCount(Repose.stringsToRecords(lines)("3491"))._1 == 42)
  }

  behavior of "mostSleepingSameMinute"
  it should "handle the example" in new SetupExampleData {
    assert(Repose.mostSleepingSameMinute(Repose.stringsToRecords(lines)) == ("99", 45, 3))
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(Repose.mostSleepingSameMinute(Repose.stringsToRecords(lines)) == ("1327", 24, 17))
  }

  class SetupExampleData {

    val lines = Seq(
      "[1518-11-01 00:00] Guard #10 begins shift",
      "[1518-11-01 00:05] falls asleep",
      "[1518-11-01 00:25] wakes up",
      "[1518-11-01 00:30] falls asleep",
      "[1518-11-01 00:55] wakes up",
      "[1518-11-01 23:58] Guard #99 begins shift",
      "[1518-11-02 00:40] falls asleep",
      "[1518-11-02 00:50] wakes up",
      "[1518-11-03 00:05] Guard #10 begins shift",
      "[1518-11-03 00:24] falls asleep",
      "[1518-11-03 00:29] wakes up",
      "[1518-11-04 00:02] Guard #99 begins shift",
      "[1518-11-04 00:36] falls asleep",
      "[1518-11-04 00:46] wakes up",
      "[1518-11-05 00:03] Guard #99 begins shift",
      "[1518-11-05 00:45] falls asleep",
      "[1518-11-05 00:55] wakes up"
    )
  }

  class SetupPuzzleData(name: String) {

    val bufferedSource: BufferedSource =
      Source.fromURL(getClass.getResource("/day4/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }

}
