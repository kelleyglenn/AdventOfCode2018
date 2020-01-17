package day18

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class LumberCollectionTest extends AnyFlatSpec {
  behavior of "run for 10 minutes"
  it should "handle the example" in {
    val area = Seq(
      ".#.#...|#.",
      ".....#|##|",
      ".|..|...#.",
      "..|#.....#",
      "#.#|||#|#|",
      "...#.||...",
      ".|....|...",
      "||...#|.#|",
      "|.||||..|.",
      "...#.|..|."
    )
    val c = new LumberCollection(area)
    (1 to 10).foreach((_: Int) => {
      c.oneMinute()
    })
    assert(c.resourceValue == 1147)
  }
  it should "solve the puzzle part 1" in new SetupData("input") {
    val c = new LumberCollection(lines)
    (1 to 10).foreach((_: Int) => {
      c.oneMinute()
    })
    assert(c.resourceValue == 589931)
  }
  it should "solve the puzzle part 2" in new SetupData("input") {
    // I've determined that the area loops every 28 "minutes" by minute 580, and 1000000000-580 is a multiple of 28,
    // which means that the result at 1000000000 will be the same
    val runSomeMinutes = 580
    val loopFreq = 28
    val c = new LumberCollection(lines)
    var target: Seq[Seq[c.Acre]] = Seq.empty
    (1 to (runSomeMinutes + loopFreq)).foreach((i: Int) => {
      c.oneMinute()
      if (i == runSomeMinutes) target = c.area
    })
    assert(c.area == target)
    assert((1000000000 - runSomeMinutes) % loopFreq == 0)
    assert(c.resourceValue == 222332)
  }

  class SetupData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day18/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }

}
