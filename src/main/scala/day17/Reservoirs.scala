package day17

import scala.collection.mutable
import scala.util.matching.Regex

class Reservoirs(tiles: Array[Array[Char]], waterSource: (Int, Int)) {
  val waterSources: mutable.Stack[WaterSource] = mutable.Stack.empty.push(DownWaterSource(waterSource._1, waterSource._2))
  while (waterSources.nonEmpty) {
    waterSources.pop() match {
      case LRWaterSource(x, y) => handleLRWaterSource(x, y)
      case DownWaterSource(x, y) => handleDownWaterSource(x, y)
    }
  }

  def printTiles(minX:Int, minY:Int): Unit = {
    print(' ')
    tiles.head.indices.map(_+minX).foreach { i: Int => print(if ((i / 10) % 10 == 0 && i % 10==0) (i / 100) % 10 else ' ') }
    println
    print(' ')
    tiles.head.indices.map(_+minX)foreach { i: Int => print(if (i % 10 == 0) (i / 10) % 10 else ' ') }
    println
    print(' ')
    tiles.head.indices.map(_+minX)foreach { i: Int => print(i % 10) }
    println
    tiles.zipWithIndex.foreach { case (s, i) => print((i+minY) % 10); println(s.mkString) }
  }

  def handleDownWaterSource(x: Int, y: Int): Unit = {
    var curY: Int = y
    while (canGoDown(x, curY)) {
      curY += 1
      tiles(curY)(x) = '|'
    }
    if (tiles.indices.contains(curY + 1)) {
      waterSources.push(LRWaterSource(x, curY))
    }
  }

  private def handleLRWaterSource(x: Int, y: Int): Unit = {
    var minX: Int = x
    while (!canGoDown(minX, y) && canGoLeft(minX, y)) {
      minX -= 1
      tiles(y)(minX) = '|'
    }
    var maxX: Int = x
    while (!canGoDown(maxX, y) && canGoRight(maxX, y)) {
      maxX += 1
      tiles(y)(maxX) = '|'
    }
    if (!canGoDown(minX, y) && !canGoDown(maxX, y)) {
      // fill this row
      while (minX < x) {
        tiles(y)(minX) = '~'
        minX += 1
      }
      while (maxX > x) {
        tiles(y)(maxX) = '~'
        maxX -= 1
      }
      tiles(y)(x) = '~'
      tiles(y - 1)(x) = '|'
      waterSources.push(DownWaterSource(x, y - 1))
    } else {
      if (canGoDown(minX, y)) {
        waterSources.push(DownWaterSource(minX, y))
      }
      if (canGoDown(maxX, y)) {
        waterSources.push(DownWaterSource(maxX, y))
      }
    }
  }

  def canGoLeft(x: Int, y: Int): Boolean = {
    !Set('#', '~').contains(tiles(y)(x - 1))
  }

  def canGoRight(x: Int, y: Int): Boolean = {
    !Set('#', '~').contains(tiles(y)(x + 1))
  }

  def canGoDown(x: Int, y: Int): Boolean = {
    tiles.indices.contains(y + 1) && !Set('#', '~').contains(tiles(y + 1)(x))
  }

  def countWetTiles(): Int = {
    tiles.map(_.count((c: Char) => c == '|' || c == '~')).sum
  }

  def countStandingWaterTiles(): Int = {
    tiles.map(_.count((c: Char) => c == '~')).sum
  }

  class WaterSource(x: Int, y: Int)

  case class LRWaterSource(x: Int, y: Int) extends WaterSource(x: Int, y: Int)

  case class DownWaterSource(x: Int, y: Int) extends WaterSource(x: Int, y: Int)

}

object Reservoirs {

  def rangesToTiles(s: (Set[XYRange], Set[YXRange])): (Array[Array[Char]], (Int, Int), (Int, Int, Int, Int)) = {
    s match {
      case (xys: Set[XYRange], yxs: Set[YXRange]) =>
        val xyStats: (Int, Int, Int, Int) = xys.foldLeft((500, 500, Int.MaxValue, Int.MinValue)) {
          case ((minX, maxX, minY, maxY), xyRange: XYRange) => (minX min xyRange.x, maxX max xyRange.x, minY min xyRange.ys.min, maxY max xyRange.ys.max)
        }
        val (minX, maxX, minY, maxY) = yxs.foldLeft(xyStats) {
          case ((minX, maxX, minY, maxY), yxRange: YXRange) => (minX min yxRange.xs.min, maxX max yxRange.xs.max, minY min yxRange.y, maxY max yxRange.y)
        } match {
          case (minX, maxX, minY, maxY) => (minX - 1, maxX + 1, minY, maxY)
        }
        val source: Array[Char]=(minX to maxX).map((i: Int) => if (i==500) '+' else ' ').toArray
        val tiles: Array[Array[Char]] = (minY to maxY).map((_: Int) => (minX to maxX).map((_: Int) => ' ').toArray).toArray
        xys.foreach { case XYRange(x, ys) => ys.foreach((y: Int) => tiles(y - minY)(x - minX) = '#') }
        yxs.foreach { case YXRange(y, xs) => xs.foreach((x: Int) => tiles(y - minY)(x - minX) = '#') }
        (source +: tiles, (500 - minX, 0), (minX, maxX, minY, maxY))
    }
  }

  def stringsToRanges(strings: Seq[String]): (Set[XYRange], Set[YXRange]) = {
    strings.foldLeft((Set[XYRange]().empty, Set[YXRange]().empty)) { case ((xys, yxs), s) =>
      stringToEitherRange(s) match {
        case Left(xy) => (xys + xy, yxs)
        case Right(yx) => (xys, yxs + yx)
      }
    }
  }

  def stringToEitherRange(s: String): Either[XYRange, YXRange] = {
    val xyRegEx: Regex = raw"x=(\d+), y=(\d+)\.\.(\d+)".r("x", "yMin", "yMax")
    val yxRegEx: Regex = raw"y=(\d+), x=(\d+)\.\.(\d+)".r("y", "xMin", "xMax")
    if (xyRegEx.matches(s)) {
      val i: Regex.MatchIterator = xyRegEx.findAllIn(s)
      Left(XYRange(i.group("x").toInt, i.group("yMin").toInt to i.group("yMax").toInt))
    } else if (yxRegEx.matches(s)) {
      val i: Regex.MatchIterator = yxRegEx.findAllIn(s)
      Right(YXRange(i.group("y").toInt, i.group("xMin").toInt to i.group("xMax").toInt))
    } else throw new IllegalStateException("Unable to parse this string to a range: " + s)
  }

  case class XYRange(x: Int, ys: Range)

  case class YXRange(y: Int, xs: Range)

}
