package day8

import org.scalatest.flatspec.AnyFlatSpec
import LicenseDecoder.Node

import scala.io.{BufferedSource, Source}

class LicenseDecoderTest extends AnyFlatSpec {
  behavior of "fromString"
  it should "handle examples" in new SetupExampleData {
    assert(Node.fromString("0 1 12") == Node(Seq.empty, Seq(12)))
    assert(Node.fromString("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2") == nodeA)
    assert(nodeA.metadataSum == 138)
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(Node.fromString(lines.head).metadataSum == 42798)
  }

  behavior of "value"
  it should "handle examples" in new SetupExampleData {
    assert(nodeB.value == 33)
    assert(nodeD.value == 99)
    assert(nodeC.value == 0)
    assert(nodeA.value == 66)
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(Node.fromString(lines.head).value == 23798)
  }

  class SetupExampleData {
    val nodeD: Node = Node(Seq.empty, Seq(99))
    val nodeC: Node = Node(Seq(nodeD), Seq(2))
    val nodeB: Node = Node(Seq.empty, Seq(10, 11, 12))
    val nodeA: Node = Node(Seq(nodeB, nodeC), Seq(1, 1, 2))
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day8/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }
}
