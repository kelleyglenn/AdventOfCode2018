package day12

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class HallOfPlantsTest extends AnyFlatSpec {
  behavior of "stringToPlants"
  it should "handle the example" in new SetupExampleData {

    assert(HallOfPlants.stringToPlants(plantsAndRules.head.split(": ")(1)) == expectedPlants)
  }

  behavior of "stringsToRules"
  it should "handle the example" in new SetupExampleData {
    assert(HallOfPlants.stringsToRules(plantsAndRules.drop(2))((false, false, false, true, true)))
    assert(HallOfPlants.stringsToRules(plantsAndRules.drop(2))((true, true, true, true, false)))
    assert(HallOfPlants.stringsToRules(plantsAndRules.drop(2)).get((true, true, true, true, true)).isEmpty)
  }

  behavior of "stringsToPlantsAndRules"
  it should "handle the example" in new SetupExampleData {
    assert(HallOfPlants.stringsToPlantsAndRules(plantsAndRules)._1 == expectedPlants)
    assert(HallOfPlants.stringsToPlantsAndRules(plantsAndRules)._2((false, false, false, true, true)))
  }

  behavior of "runGenerations"
  it should "handle the example" in new SetupExampleData {

    val params: (Seq[Boolean], Map[(Boolean, Boolean, Boolean, Boolean, Boolean), Boolean]) =
      HallOfPlants.stringsToPlantsAndRules(plantsAndRules)
    val hall = new HallOfPlants(params._1, params._2)
    assert(hall.mkString == "#..#.#..##......###...###")
    hall.runGenerations(1)
    assert(hall.mkString == "#...#....#.....#..#..#..#")
    hall.runGenerations(19)
    assert(hall.mkString == "#....##....#####...#######....#.#..##")
    assert(hall.numbersOfPotsWithPlants.sum == 325)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {

    val params: (Seq[Boolean], Map[(Boolean, Boolean, Boolean, Boolean, Boolean), Boolean]) =
      HallOfPlants.stringsToPlantsAndRules(lines)
    val hall = new HallOfPlants(params._1, params._2)
    hall.runGenerations(20)
    assert(hall.numbersOfPotsWithPlants.sum == 2045)
  }

  class SetupExampleData {

    val expectedPlants = Seq(true, false, false, true, false, true, false, false, true, true, false, false, false,
      false, false, false, true, true, true, false, false, false, true, true, true)

    val plantsAndRules = Seq(
      "initial state: #..#.#..##......###...###",
      "",
      "...## => #",
      "..#.. => #",
      ".#... => #",
      ".#.#. => #",
      ".#.## => #",
      ".##.. => #",
      ".#### => #",
      "#.#.# => #",
      "#.### => #",
      "##.#. => #",
      "##.## => #",
      "###.. => #",
      "###.# => #",
      "####. => #"
    )
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day12/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }

}
