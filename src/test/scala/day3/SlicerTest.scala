package day3

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class SlicerTest extends AnyFlatSpec {
  behavior of "Rect.fromString"
  it should "handle examples" in {
    assert(Rect.fromString("3,2: 5x4") == Rect(2, 6, 3, 8))
  }
  it should "be reflexive" in {
    assert(Rect.fromString("3,2: 5x4").toString == "3,2: 5x4")
  }

  behavior of "Claim.toString"
  it should "handle examples" in {
    assert(Claim.fromString("#123 @ 3,2: 5x4") == Claim(123, Rect(2, 6, 3, 8)))
  }
  it should "be reflexive" in {
    assert(Claim.fromString("#123 @ 3,2: 5x4").toString == "#123 @ 3,2: 5x4")
  }

  behavior of "intersection"
  it should "handle examples" in new SetupExampleData {
    assert(r1.intersection(r3).isEmpty)
    assert(r3.intersection(r1).isEmpty)
    assert(r2.intersection(r3).isEmpty)
    assert(r3.intersection(r2).isEmpty)
    assert(r1.intersection(r2).contains(Rect(3, 5, 3, 5)))
    assert(r2.intersection(r1).contains(Rect(3, 5, 3, 5)))
    assert(r1.intersection(r1).contains(r1))
  }

  behavior of "totalArea"
  it should "handle examples" in new SetupExampleData {
    assert(Rect.totalArea(Set(r1)) == 16)
    assert(Rect.totalArea(Set(r2)) == 16)
    assert(Rect.totalArea(Set(r1, r2)) == 28)
    assert(Rect.totalArea(Set(r1.intersection(r2).get)) == 4)
  }

  behavior of "intersections"
  it should "handle examples" in {
    val claims: Set[Claim] =
      Set(
        Claim.fromString("#1 @ 1,3: 4x4"),
        Claim.fromString("#2 @ 3,1: 4x4"),
        Claim.fromString("#3 @ 5,5: 2x2")
      )
    assert(Slicer.intersections(claims) == Set(Rect(3, 5, 3, 5)))
  }

  behavior of "totalOverlappingArea"
  it should "handle examples" in new SetupExampleData {
    assert(Slicer.totalOverlappingArea(Set(c1, c2, c3)) == 4)
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(Slicer.totalOverlappingArea(claims) == 111935)
  }

  behavior of "distinctClaims"
  it should "handle examples" in new SetupExampleData {
    assert(Slicer.distinctClaims(Set(c1, c2, c3)) == Set(c3))
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(
      Slicer.distinctClaims(claims) == Set(
        Claim.fromString("#650 @ 830,151: 25x21")
      )
    )
  }

  class SetupExampleData {
    val r1: Rect = Rect.fromString("1,3: 4x4")
    val r2: Rect = Rect.fromString("3,1: 4x4")
    val r3: Rect = Rect.fromString("5,5: 2x2")
    val c1: Claim = Claim.fromString("#1 @ 1,3: 4x4")
    val c2: Claim = Claim.fromString("#2 @ 3,1: 4x4")
    val c3: Claim = Claim.fromString("#3 @ 5,5: 2x2")
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource =
      Source.fromURL(getClass.getResource("/day3/" + name + ".txt"))
    val claims: Set[Claim] =
      bufferedSource.getLines.toSeq.map(Claim.fromString).toSet
  }

}
