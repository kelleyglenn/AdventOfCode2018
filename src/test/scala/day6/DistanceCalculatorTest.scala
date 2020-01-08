package day6

import day6.DistanceCalculator.Point
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class DistanceCalculatorTest extends AnyFlatSpec {
  behavior of "stringsToPointsWithIndex"
  it should "handle examples" in new SetupExampleData {
    assert(DistanceCalculator.stringsToPointsWithIndex(strings) == points)
  }

  behavior of "closestPoint"
  it should "handle examples" in new SetupExampleData {
    assert(DistanceCalculator.closestPoint(points, Point(0, 0)).contains(0))
    assert(DistanceCalculator.closestPoint(points, Point(1, 1)).contains(0))
    assert(DistanceCalculator.closestPoint(points, Point(8, 3)).contains(2))
    assert(DistanceCalculator.closestPoint(points, Point(5, 0)).isEmpty)
  }

  behavior of "pointsToGrid"
  it should "handle simple example" in {
    assert(
      DistanceCalculator.pointsToGrid(Seq((Point(0, 0), 12), (Point(0, 1), 34)), DistanceCalculator.closestPoint) ==
        Seq(Seq(Some(12), Some(34))))
    assert(
      DistanceCalculator.pointsToGrid(Seq((Point(0, 0), 12), (Point(1, 1), 34)), DistanceCalculator.closestPoint) ==
        Seq(Seq(Some(12), None), Seq(None, Some(34))))
  }
  it should "handle full example" in new SetupExampleData {
    val result: Seq[Seq[Option[Short]]] = DistanceCalculator.pointsToGrid(points, DistanceCalculator.closestPoint)
    assert(result.size == 8)
    assert(result.head.size == 9)
    assert(result.head == Seq(Some(0), Some(0), Some(0), None, Some(1), Some(1), Some(1), Some(1), Some(1)))
    assert(result(1) == Seq(Some(0), Some(0), Some(3), Some(3), None, Some(1), Some(1), Some(1), Some(1)))
  }

  behavior of "isValueAlongGridEdge"
  it should "handle example" in new SetupExampleData {
    val grid: Seq[Seq[Option[Short]]] = DistanceCalculator.pointsToGrid(points, DistanceCalculator.closestPoint)
    assert(DistanceCalculator.isValueAlongGridEdge(grid, Some(0)))
    assert(DistanceCalculator.isValueAlongGridEdge(grid, Some(1)))
    assert(DistanceCalculator.isValueAlongGridEdge(grid, Some(2)))
    assert(!DistanceCalculator.isValueAlongGridEdge(grid, Some(3)))
    assert(!DistanceCalculator.isValueAlongGridEdge(grid, Some(4)))
    assert(DistanceCalculator.isValueAlongGridEdge(grid, Some(5)))
  }

  behavior of "gridToCounts"
  it should "handle example" in new SetupExampleData {
    val grid: Seq[Seq[Option[Short]]] = DistanceCalculator.pointsToGrid(points, DistanceCalculator.closestPoint)
    assert(DistanceCalculator.gridToCounts(grid) == Map(0 -> 7, 1 -> 9, 2 -> 12, 3 -> 9, 4 -> 17, 5 -> 10))
  }

  behavior of "largestFiniteArea"
  it should "handle example" in new SetupExampleData {
    val grid: Seq[Seq[Option[Short]]] = DistanceCalculator.pointsToGrid(points, DistanceCalculator.closestPoint)
    assert(DistanceCalculator.largestFiniteArea(points) == 17)
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(DistanceCalculator.largestFiniteArea(DistanceCalculator.stringsToPointsWithIndex(lines)) == 5358)
  }

  behavior of "countTotalDistances"
  it should "handle example" in new SetupExampleData {
    assert(DistanceCalculator.countTotalDistances(points, _ < 32) == 16)
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(
      DistanceCalculator
        .countTotalDistances(DistanceCalculator.stringsToPointsWithIndex(lines), _ < 10000) == 37093)
  }

  class SetupExampleData {
    val strings: Seq[String] = Seq("1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9")

    val points: Seq[(Point, Short)] =
      Seq((Point(1, 1), 0), (Point(1, 6), 1), (Point(8, 3), 2), (Point(3, 4), 3), (Point(5, 5), 4), (Point(8, 9), 5))
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day6/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }

}
