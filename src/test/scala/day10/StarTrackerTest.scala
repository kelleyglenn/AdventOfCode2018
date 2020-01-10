package day10

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class StarTrackerTest extends AnyFlatSpec {
  behavior of "stringsToStars"
  it should "handle simple" in {
    assert(StarTracker.stringsToStars(Seq("position=<-3, -2> velocity=<-1, -1>")) == Set(((-3, -2), (-1, -1))))
    assert(StarTracker.stringsToStars(Seq("position=< 3,  2> velocity=< 1,  1>")) == Set(((3, 2), (1, 1))))
  }
  it should "handle the example" in new SetupExampleData {
    assert(origStars.contains(((9, 1), (0, 2))))
    assert(origStars.contains(((-3, 6), (2, -1))))
  }

  behavior of "fastForward"
  it should "handle the example" in new SetupExampleData {
    assert(StarTracker.fastForward(origStars, 0) == origStars)
    assert(StarTracker.fastForward(origStars, 1).contains(((9, 3), (0, 2))))
    assert(StarTracker.fastForward(origStars, 1).contains(((-1, 5), (2, -1))))
    assert(StarTracker.fastForward(origStars, 4).contains(((9, 9), (0, 2))))
    assert(StarTracker.fastForward(origStars, 4).contains(((5, 2), (2, -1))))
  }

  behavior of "spread"
  it should "handle the example" in new SetupExampleData {
    val stars1: Set[((Int, Int), (Int, Int))] = StarTracker.fastForward(origStars, 1)
    val stars2: Set[((Int, Int), (Int, Int))] = StarTracker.fastForward(origStars, 2)
    val stars3: Set[((Int, Int), (Int, Int))] = StarTracker.fastForward(origStars, 3)
    val stars4: Set[((Int, Int), (Int, Int))] = StarTracker.fastForward(origStars, 4)
    assert(StarTracker.spread(origStars) > StarTracker.spread(stars1))
    assert(StarTracker.spread(stars1) > StarTracker.spread(stars2))
    assert(StarTracker.spread(stars2) > StarTracker.spread(stars3))
    assert(StarTracker.spread(stars3) < StarTracker.spread(stars4))
  }
  it should "handle some extremes" in new SetupExampleData {
    assert(StarTracker.spread(origStars) < StarTracker.spread(StarTracker.fastForward(origStars, -100)))
    assert(StarTracker.spread(origStars) < StarTracker.spread(StarTracker.fastForward(origStars, 100)))
    assert(StarTracker.spread(origStars) < StarTracker.spread(StarTracker.fastForward(origStars, -1000)))
    assert(StarTracker.spread(origStars) < StarTracker.spread(StarTracker.fastForward(origStars, 1000)))
  }

  behavior of "starsToStrings"
  it should "handle the example" in new SetupExampleData {

    val expected: Seq[String] = Seq(
      "........#.............",
      "................#.....",
      ".........#.#..#.......",
      "......................",
      "#..........#.#.......#",
      "...............#......",
      "....#.................",
      "..#.#....#............",
      ".......#..............",
      "......#...............",
      "...#...#.#...#........",
      "....#..#..#.........#.",
      ".......#..............",
      "...........#..#.......",
      "#...........#.........",
      "...#.......#.........."
    )
    assert(StarTracker.starsToStrings(origStars) == expected)
    StarTracker
      .starsToStrings(StarTracker.fastForward(origStars, 3))
      .foreach((s: String) => println(s.replace('.', ' ')))
  }

  behavior of "secondsToMinSpread"
  it should "handle the example" in new SetupExampleData {
    assert(StarTracker.secondsToMinSpread(origStars) == 3)
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    val puzzleStars: Set[((Int, Int), (Int, Int))] = StarTracker.stringsToStars(lines)
    assert(StarTracker.secondsToMinSpread(puzzleStars) == 10595)
    StarTracker
      .starsToStrings(StarTracker.fastForward(puzzleStars, 10595))
      .foreach((s: String) => println(s.replace('.', ' ')))
  }

  class SetupExampleData {

    val strings: Seq[String] = Seq(
      "position=< 9,  1> velocity=< 0,  2>",
      "position=< 7,  0> velocity=<-1,  0>",
      "position=< 3, -2> velocity=<-1,  1>",
      "position=< 6, 10> velocity=<-2, -1>",
      "position=< 2, -4> velocity=< 2,  2>",
      "position=<-6, 10> velocity=< 2, -2>",
      "position=< 1,  8> velocity=< 1, -1>",
      "position=< 1,  7> velocity=< 1,  0>",
      "position=<-3, 11> velocity=< 1, -2>",
      "position=< 7,  6> velocity=<-1, -1>",
      "position=<-2,  3> velocity=< 1,  0>",
      "position=<-4,  3> velocity=< 2,  0>",
      "position=<10, -3> velocity=<-1,  1>",
      "position=< 5, 11> velocity=< 1, -2>",
      "position=< 4,  7> velocity=< 0, -1>",
      "position=< 8, -2> velocity=< 0,  1>",
      "position=<15,  0> velocity=<-2,  0>",
      "position=< 1,  6> velocity=< 1,  0>",
      "position=< 8,  9> velocity=< 0, -1>",
      "position=< 3,  3> velocity=<-1,  1>",
      "position=< 0,  5> velocity=< 0, -1>",
      "position=<-2,  2> velocity=< 2,  0>",
      "position=< 5, -2> velocity=< 1,  2>",
      "position=< 1,  4> velocity=< 2,  1>",
      "position=<-2,  7> velocity=< 2, -2>",
      "position=< 3,  6> velocity=<-1, -1>",
      "position=< 5,  0> velocity=< 1,  0>",
      "position=<-6,  0> velocity=< 2,  0>",
      "position=< 5,  9> velocity=< 1, -2>",
      "position=<14,  7> velocity=<-2,  0>",
      "position=<-3,  6> velocity=< 2, -1>"
    )
    val origStars: Set[((Int, Int), (Int, Int))] = StarTracker.stringsToStars(strings)
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day10/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }
}
