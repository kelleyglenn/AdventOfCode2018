package day3

import scala.util.matching.Regex

object Slicer {
  def intersections(claim: Claim, claims: Set[Claim]): Set[Rect] = {
    if (claims.isEmpty) Set.empty
    else
      claim.rect.intersection(claims.head.rect) match {
        case Some(i) => intersections(claim, claims.tail) + i
        case _       => intersections(claim, claims.tail)
      }
  }
  def intersections(claims: Set[Claim]): Set[Rect] = {
    if (claims.isEmpty) Set.empty
    else intersections(claims.head, claims.tail) ++ intersections(claims.tail)
  }

  def totalOverlappingArea(claims: Set[Claim]): Int = {
    Rect.totalArea(intersections(claims))
  }

  def distinctClaims(claims: Set[Claim]): Set[Claim] = {
    claims.filter { c: Claim =>
      !claims.exists((other: Claim) => {
        other != c && c.rect.intersection(other.rect).isDefined
      })
    }
  }
}

case class Claim(id: Int, rect: Rect) {
  override def toString: String = "#" + id + " @ " + rect
}

object Claim {
  def fromString(s: String): Claim = {
    val groups: Regex.MatchIterator = raw"#(\d+) @ (.*)"
      .r("id", "rect")
      .findAllIn(s)
    Claim(groups.group("id").toInt, Rect.fromString(groups.group("rect")))
  }
}

case class Rect(top: Int, bottom: Int, left: Int, right: Int) {
  assert(top < bottom)
  assert(left < right)

  def intersection(other: Rect): Option[Rect] = {
    val maxTop: Int = Math.max(top, other.top)
    val minBottom: Int = Math.min(bottom, other.bottom)
    val maxLeft: Int = math.max(left, other.left)
    val minRight: Int = math.min(right, other.right)
    if (maxTop >= minBottom || maxLeft >= minRight) None
    else Some(Rect(maxTop, minBottom, maxLeft, minRight))
  }

  def area: Int = (bottom - top) * (right - left)

  def atoms: Set[(Int, Int)] = {
    (for (y <- top until bottom; x <- left until right) yield (x, y)).toSet
  }

  override def toString: String =
    "" + left + "," + top + ": " + (right - left) + "x" + (bottom - top)
}

object Rect {
  def fromString(s: String): Rect = {
    val groups: Regex.MatchIterator = raw"(\d+),(\d+): (\d+)x(\d+)"
      .r("left", "top", "width", "height")
      .findAllIn(s)
    val left: Int = groups.group("left").toInt
    val top: Int = groups.group("top").toInt
    val width: Int = groups.group("width").toInt
    val height: Int = groups.group("height").toInt
    Rect(top, top + height, left, left + width)
  }

  def totalArea(rects: Set[Rect]): Int = {
    rects.flatMap(_.atoms).size
  }
}
