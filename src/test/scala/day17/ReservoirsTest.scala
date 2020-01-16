package day17

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class ReservoirsTest extends AnyFlatSpec {
  behavior of "stringsToRanges"
  it should "handle example 1" in new SetupData("example1") {
    Reservoirs.stringsToRanges(lines) match {
      case (xys, yxs) =>
        assert(xys.contains(Reservoirs.XYRange(495, 2 to 7)))
        assert(yxs.contains(Reservoirs.YXRange(13, 498 to 504)))
    }
  }

  behavior of "rangesToTiles"
  it should "handle example 1" in new SetupData("example1") {
    Reservoirs.rangesToTiles(Reservoirs.stringsToRanges(lines)) match {
      case (tiles, _, stats) =>
        assert(stats == (494, 507, 1, 13))
        assert((tiles.head.length, tiles.length) == (14, 14))
        assert(tiles.map(_.count((c: Char) => c == '+')).sum == 1)
        assert(tiles.map(_.count((c: Char) => c == '#')).sum == 34)
        assert(tiles.last.mkString == "    #######   ")
    }
  }
  it should "handle the puzzle" in new SetupData("input") {
    Reservoirs.rangesToTiles(Reservoirs.stringsToRanges(lines)) match {
      case (tiles, _, stats) =>
        assert(stats == (403, 629, 6, 1631))
        assert((tiles.head.length, tiles.length) == (227, 1627))
    }
  }

  behavior of "countWetTiles"
  it should "handle example 1" in new SetupData("example1") {
    Reservoirs.rangesToTiles(Reservoirs.stringsToRanges(lines)) match {
      case (tiles, waterSource, _) =>
        val res = new Reservoirs(tiles, waterSource)
        assert(res.countWetTiles() == 57)
    }
  }
  it should "solve the puzzle" in new SetupData("input") {
    val tilesWater: (Array[Array[Char]], (Int, Int), (Int, Int, Int, Int)) = Reservoirs.rangesToTiles(Reservoirs.stringsToRanges(lines))
    tilesWater match {
      case (tiles: Array[Array[Char]], waterSource, _) =>
        val res = new Reservoirs(tiles, waterSource)
        assert(res.countWetTiles() == 30635)
    }
  }

  behavior of "countStandingWaterTiles"
  it should "handle example 1" in new SetupData("example1") {
    Reservoirs.rangesToTiles(Reservoirs.stringsToRanges(lines)) match {
      case (tiles, waterSource, _) =>
        val res = new Reservoirs(tiles, waterSource)
        assert(res.countStandingWaterTiles() == 29)
    }
  }
  it should "solve the puzzle" in new SetupData("input") {
    val tilesWater: (Array[Array[Char]], (Int, Int), (Int, Int, Int, Int)) = Reservoirs.rangesToTiles(Reservoirs.stringsToRanges(lines))
    tilesWater match {
      case (tiles: Array[Array[Char]], waterSource, _) =>
        val res = new Reservoirs(tiles, waterSource)
        assert(res.countStandingWaterTiles() == 25094)
    }
  }

  class SetupData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day17/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }

}
