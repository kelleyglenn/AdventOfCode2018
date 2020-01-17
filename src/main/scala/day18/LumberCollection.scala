package day18

class LumberCollection(val origArea: Seq[String]) {
  var area: Seq[Seq[Acre]] = origArea.map(_.toSeq)

  def oneMinute(): Unit = {
    area = for (y <- area.indices) yield {
      for (x <- area(y).indices) yield {
        change(area(y)(x), neighborsOf(y, x))
      }
    }
  }

  def change(from: Acre, neighbors: Seq[Acre]): Acre = {
    from match {
      case Open => if (neighbors.count(_ == Trees) >= 3) Trees else Open
      case Trees => if (neighbors.count(_ == Yard) >= 3) Yard else Trees
      case Yard => if (neighbors.contains(Yard) && neighbors.contains(Trees)) Yard else Open
    }
  }

  type Acre = Char
  val Open: Acre = '.'
  val Trees: Acre = '|'
  val Yard: Acre = '#'

  def neighborsOf(y: Int, x: Int): Seq[Acre] =
    Seq(-1, 0, 1).flatMap((xd: Int) => Range.inclusive(-1, 1, if (xd == 0) 2 else 1).map((yd: Int) => neighborAt(y + yd, x + xd))).flatten

  def neighborAt(y: Int, x: Int): Option[Acre] = {
    if (area.indices.contains(y) && area.head.indices.contains(x)) Some(area(y)(x)) else None
  }

  def resourceValue: Long = {
    area.map(_.count(_ == Trees).toLong).sum * area.map(_.count(_ == Yard).toLong).sum
  }
}
