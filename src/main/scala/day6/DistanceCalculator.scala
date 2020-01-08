package day6

import scala.reflect.ClassTag

object DistanceCalculator {
  case class Point(x: Int, y: Int)

  def stringsToPointsWithIndex(strings: Seq[String]): Seq[(Point, Short)] = {
    strings
      .map((s: String) => s.splitAt(s.indexOf(", ")))
      .map((p: (String, String)) => Point(p._1.toInt, p._2.drop(2).toInt))
      .zipWithIndex
      .map((p: (Point, Int)) => (p._1, p._2.toShort))
  }

  def distanceBetween(a: Point, b: Point): Int = {
    (a.x - b.x).abs + (a.y - b.y).abs
  }

  def closestPoint(points: Seq[(Point, Short)], p: Point): Option[Short] = {
    val distances: Seq[(Int, Short)] =
      points.map((ps: (Point, Short)) => (distanceBetween(ps._1, p), ps._2)).sorted
    if (distances.head._1 == distances(1)._1) None
    else Some(distances.head._2)
  }

  def totalDistance(points: Seq[(Point, Short)], p: Point): Int = {
    points.map((ps: (Point, Short)) => distanceBetween(ps._1, p)).sum
  }

  def pointsToGrid[T: ClassTag](points: Seq[(Point, Short)], f: (Seq[(Point, Short)], Point) => T): Seq[Seq[T]] = {
    val minX: Int = points.map(_._1.x).min
    val maxX: Int = points.map(_._1.x).max
    val minY: Int = points.map(_._1.y).min
    val maxY: Int = points.map(_._1.y).max
    val closestPoints: Array[Array[T]] = Array.ofDim[T](maxX - minX + 1, maxY - minY + 1)
    for (x <- closestPoints.indices) {
      for (y <- closestPoints(0).indices) {
        closestPoints(x)(y) = f(points, Point(x + minX, y + minY))
      }
    }
    closestPoints.map(_.toSeq).toSeq
  }

  def isValueAlongGridEdge[T: ClassTag](graph: Seq[Seq[T]], value: T): Boolean = {
    graph.head.contains(value) ||
    graph.last.contains(value) ||
    (for (i <- graph.indices) yield graph(i).head).contains(value) ||
    (for (i <- graph.indices) yield graph(i).last).contains(value)
  }

  def gridToCounts(grid: Seq[Seq[Option[Short]]]): Map[Short, Int] = {
    grid.flatten
      .collect { case Some(x) => x }
      .groupBy((s: Short) => s)
      .map((count: (Short, Seq[Short])) => (count._1, count._2.size))
  }

  def largestFiniteArea(points: Seq[(Point, Short)]): Int = {
    val grid: Seq[Seq[Option[Short]]] = pointsToGrid(points, closestPoint)
    val counts: Map[Short, Int] = gridToCounts(grid)
    points.map(_._2).filterNot((s: Short) => isValueAlongGridEdge(grid, Some(s))).map(counts(_)).max
  }

  def countTotalDistances(points: Seq[(Point, Short)], p: Int => Boolean): Int = {
    (for {
      x <- points.map(_._1.x).min to points.map(_._1.x).max
      y <- points.map(_._1.y).min to points.map(_._1.y).max
    } yield totalDistance(points, Point(x, y))).count(p)
  }
}
