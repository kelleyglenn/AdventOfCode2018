package day20

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class MapperTest extends AnyFlatSpec {
  behavior of "visitedLocationDistances"
  it should "handle example 1" in {
    assert(Mapper.visitedLocationDistances("^WNE$") ==
      LocationOrganizer(Map(Location(0, 0) -> 0, Location(-1, 0) -> 1, Location(-1, 1) -> 2, Location(0, 1) -> 3)))
  }
  it should "handle example 2" in {
    assert(Mapper.visitedLocationDistances("^ENWWW(NEEE|SSE(EE|N))$").locations.contains(Location(1, -1)))
    assert(Mapper.visitedLocationDistances("^ENWWW(NEEE|SSE(EE|N))$").locations(Location(1, -1)) == 10)
  }
  it should "handle example 3" in {
    assert(Mapper.visitedLocationDistances("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$").locations(Location(2, 2)) == 18)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(Mapper.visitedLocationDistances(lines.head).locations.count(_._2 >= 1000) == 8366)
  }

  behavior of "distanceToFarthestRoom"
  it should "handle example 1" in {
    assert(Mapper.distanceToFarthestRoom("^WNE$") == 3)
  }
  it should "handle example 2" in {
    assert(Mapper.distanceToFarthestRoom("^ENWWW(NEEE|SSE(EE|N))$") == 10)
  }
  it should "handle example 3" in {
    assert(Mapper.distanceToFarthestRoom("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$") == 18)
  }
  it should "handle example 4" in {
    assert(Mapper.distanceToFarthestRoom("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$") == 23)
  }
  it should "handle example 5" in {
    assert(Mapper.distanceToFarthestRoom("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$") == 31)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(Mapper.distanceToFarthestRoom(lines.head) == 4108)
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day20/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }

}
