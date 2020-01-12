package day13

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class MineTracksTest extends AnyFlatSpec {
  behavior of "constructor"
  it should "handle example 1" in new SetupData("example1") {
    assert(new MineTracks(lines).carts == Set(Cart((0, 1), Direction.D, Turn.L), Cart((0, 5), Direction.U, Turn.L)))
  }
  it should "handle example 2" in new SetupData("example2") {
    assert(new MineTracks(lines).carts == Set(Cart((2, 0), Direction.R, Turn.L), Cart((9, 3), Direction.D, Turn.L)))
  }
  it should "handle the puzzle" in new SetupData("input") {

    val expected = Set(
      Cart((142, 13), Direction.D, Turn.L),
      Cart((22, 19), Direction.U, Turn.L),
      Cart((98, 110), Direction.L, Turn.L),
      Cart((47, 147), Direction.R, Turn.L))
    assert(expected.subsetOf(new MineTracks(lines).carts))
  }

  behavior of "tick"
  it should "handle example 1" in new SetupData("example1") {
    val tracks = new MineTracks(lines)
    tracks.tick()
    assert(tracks.carts == Set(Cart((0, 2), Direction.D, Turn.L), Cart((0, 4), Direction.U, Turn.L)))
    tracks.tick()
    assert(tracks.carts == Set(Cart((0, 3), Direction.D, Turn.L), Cart((0, 3), Direction.U, Turn.L)))
    assert(tracks.locationOfFirstCrash.contains((0, 3)))
  }
  it should "handle example 2" in new SetupData("example2") {
    val tracks = new MineTracks(lines)
    tracks.tick()
    tracks.tick()
    assert(tracks.carts == Set(Cart((4, 0), Direction.D, Turn.L), Cart((10, 4), Direction.R, Turn.S)))
  }

  behavior of "tickUntilCrash"
  it should "handle example 1" in new SetupData("example1") {
    val tracks = new MineTracks(lines)
    assert(tracks.tickUntilFirstCrash() == (0, 3))
  }
  it should "handle example 2" in new SetupData("example2") {
    val tracks = new MineTracks(lines)
    assert(tracks.tickUntilFirstCrash() == (7, 3))
  }
  it should "solve the puzzle" in new SetupData("input") {
    val tracks = new MineTracks(lines)
    assert(tracks.tickUntilFirstCrash() == (136, 36))
  }

  class SetupData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day13/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }
}
