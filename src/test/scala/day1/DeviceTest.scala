package day1

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class DeviceTest extends AnyFlatSpec {
  behavior of "resultingFrequency"
  it should "solve the puzzle " in new SetupPuzzleData("input") {
    assert(Device.resultingFrequency(adjustments) == 529)
  }

  behavior of "firstFreqReachedTwice"
  it should "handle examples" in {
    assert(Device.firstFreqReachedTwice(Seq(+1, -2, +3, +1)) == 2)
    assert(Device.firstFreqReachedTwice(Seq(+1, -1)) == 0)
    assert(Device.firstFreqReachedTwice(Seq(+3, +3, +4, -2, -4)) == 10)
    assert(Device.firstFreqReachedTwice(Seq(-6, +3, +8, +5, -6)) == 5)
    assert(Device.firstFreqReachedTwice(Seq(+7, +7, -2, -7, -4)) == 14)
  }

  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(Device.firstFreqReachedTwice(adjustments) == 464)
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource =
      Source.fromURL(getClass.getResource("/day1/" + name + ".txt"))
    val adjustments: Seq[Int] =
      bufferedSource.getLines.toSeq.map((l: String) => { l.toInt })
  }
}
