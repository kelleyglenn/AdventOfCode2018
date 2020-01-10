package day10

import scala.util.matching.Regex

object StarTracker {

  def stringsToStars(strings: Seq[String]): Set[((Int, Int), (Int, Int))] = {
    val regEx: Regex = raw"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>".r("pX", "pY", "vX", "vY")
    strings.map { s: String =>
      val groups: Regex.MatchIterator = regEx.findAllIn(s)
      ((groups.group("pX").toInt, groups.group("pY").toInt), (groups.group("vX").toInt, groups.group("vY").toInt))
    }.toSet
  }

  def starsToStrings(stars: Set[((Int, Int), (Int, Int))]): Seq[String] = {
    val positions: Set[(Int, Int)] = stars.map(_._1)
    val minX: Int = positions.minBy(_._1)._1
    val maxX: Int = positions.maxBy(_._1)._1
    val minY: Int = positions.minBy(_._2)._2
    val maxY: Int = positions.maxBy(_._2)._2
    (minY to maxY).map { y: Int =>
      val starsThisRow: Set[Int] = positions.filter(_._2 == y).map(_._1 - minX)
      val sb = new StringBuilder()
      (0 to maxX - minX).foreach { x: Int =>
        sb.append(if (starsThisRow.contains(x)) '#' else '.')
      }
      sb.mkString
    }
  }

  def fastForward(stars: Set[((Int, Int), (Int, Int))], seconds: Int): Set[((Int, Int), (Int, Int))] = {
    stars.map((s: ((Int, Int), (Int, Int))) => ((s._1._1 + s._2._1 * seconds, s._1._2 + s._2._2 * seconds), s._2))
  }

  def medianPoint(stars: Set[((Int, Int), (Int, Int))]): (Int, Int) = {
    val xPos: Set[Int] = stars.map(_._1._1)
    val yPos: Set[Int] = stars.map(_._1._2)
    (xPos.sum / xPos.size, yPos.sum / yPos.size)
  }

  def distanceFrom(p1: (Int, Int), p2: (Int, Int)): Double = {
    Math.sqrt(Math.pow(p1._1 - p2._1, 2) + Math.pow(p1._2 - p2._2, 2))
  }

  def totalDistanceFrom(stars: Set[((Int, Int), (Int, Int))], point: (Int, Int)): Double = {
    stars.map((s: ((Int, Int), (Int, Int))) => distanceFrom(s._1, point)).sum
  }

  def spread(stars: Set[((Int, Int), (Int, Int))]): Double = {
    totalDistanceFrom(stars, medianPoint(stars))
  }

  def secondsToMinSpread(stars: Set[((Int, Int), (Int, Int))]): Int = {
    var curSec: Int = 0
    var curPM: Int = spread(stars).toInt / stars.size
    while (curPM > 0) {
      curSec = Set(curSec, curSec - curPM, curSec + curPM).minBy((s: Int) => spread(fastForward(stars, s)))
      curPM = curPM / 2
    }
    curSec
  }
}
