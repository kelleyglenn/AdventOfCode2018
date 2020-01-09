package day9

class MarbleGame(lastMarbleValue: Int, playerCount: Int) {
  var freeMarbles: Seq[Int] = 1 to lastMarbleValue
  var curMarble: Node = new Node(0, null, null)
  curMarble.clockwise = curMarble
  curMarble.counterClockwise = curMarble
  val scores: Array[Long] = new Array[Long](playerCount)
  var curPlayer: Int = 0

  private def placeNextMarble(): Unit = {
    if (freeMarbles.head % 23 == 0) {
      curMarble =
        curMarble.counterClockwise.counterClockwise.counterClockwise.counterClockwise.counterClockwise.counterClockwise.counterClockwise
      scores(curPlayer) += freeMarbles.head + curMarble.value
      curMarble.counterClockwise.clockwise = curMarble.clockwise
      curMarble.clockwise.counterClockwise = curMarble.counterClockwise
      curMarble = curMarble.clockwise
    }
    else {
      curMarble = new Node(freeMarbles.head, curMarble.clockwise, curMarble.clockwise.clockwise)
      curMarble.counterClockwise.clockwise = curMarble
      curMarble.clockwise.counterClockwise = curMarble
    }
    freeMarbles = freeMarbles.tail
    curPlayer = (curPlayer + 1) % playerCount
  }

  def play: Long = {
    while (freeMarbles.nonEmpty) placeNextMarble()
    scores.max
  }
  class Node(val value: Int, var counterClockwise: Node, var clockwise: Node)
}
