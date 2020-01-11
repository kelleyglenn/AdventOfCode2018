package day12

import java.time.LocalDateTime

class HallOfPlants(val plants: Seq[Boolean], val rules: Map[(Boolean, Boolean, Boolean, Boolean, Boolean), Boolean]) {

  var (firstPot, lastPot: Pot) = Pot.fromSeq(plants, 0)
  var centerPot: Pot = firstPot

  private def generateBufferSeq(size: Int): Seq[Boolean] = {
    (1 to size).map((_: Int) => false)
  }

  private def addBuffer(size: Int): Unit = {
    val newPre: (Pot, Pot) = Pot.fromSeq(generateBufferSeq(size), firstPot.number - size)
    val newPost: (Pot, Pot) = Pot.fromSeq(generateBufferSeq(size), lastPot.number + 1)
    newPre._2.right = firstPot
    firstPot.left = newPre._2
    firstPot = newPre._1
    lastPot.right = newPost._1
    newPost._1.left = lastPot
    lastPot = newPost._2
  }

  private def trim(): Unit = {
    while (!firstPot.value) firstPot = firstPot.right
    firstPot.left = null
    while (!lastPot.value) lastPot = lastPot.left
    lastPot.right = null
  }

  def runGenerations(gens: Long): Unit = {
    addBuffer(4)
    var curGen: Long = 0
    var previousGen: Pot = firstPot
    while (curGen < gens) {
      var pm2: Pot = firstPot
      var pm1: Pot = pm2.right
      var p: Pot = pm1.right
      var pp1: Pot = p.right
      var pp2: Pot = pp1.right
      firstPot = Pot(pm2.value, pm2.number)
      lastPot = Pot(pm1.value, pm1.number)
      firstPot.right = lastPot
      lastPot.left = firstPot
      while (pp2 != null) {
        lastPot =
          Pot(rules.getOrElse((pm2.value, pm1.value, p.value, pp1.value, pp2.value), false), p.number, lastPot, null)
        lastPot.left.right = lastPot
        pm2 = pm1; pm1 = p; p = pp1; pp1 = pp2; pp2 = pp2.right
      }
      lastPot = Pot(p.value, p.number, lastPot, null)
      lastPot.left.right = lastPot
      lastPot = Pot(pp1.value, pp1.number, lastPot, null)
      lastPot.left.right = lastPot
      addBuffer(2)
      curGen += 1
      if (curGen % 5L == 0L) {
        trim()
        addBuffer(4)
      }
    }
    trim()
  }

  def mkString: String = {
    Pot.toSeq(firstPot).map((p: (Boolean, Int)) => if (p._1) '#' else '.').mkString
  }

  def numbersOfPotsWithPlants: Seq[Int] =
    Pot.toSeq(firstPot).filter(_._1 == true).map((p: (Boolean, Int)) => p._2)
}

object HallOfPlants {

  def stringToPlants(plants: String): Seq[Boolean] = {
    plants.map(_ == '#')
  }

  def stringsToRules(rules: Seq[String]): Map[(Boolean, Boolean, Boolean, Boolean, Boolean), Boolean] = {
    rules.map { r: String =>
      val rParts: Array[String] = r.split(raw" => ")
      val key: Seq[Boolean] = rParts(0).map(_ == '#')
      ((key.head, key(1), key(2), key(3), key(4)), rParts(1) == "#")
    }.toMap
  }

  def stringsToPlantsAndRules(
      strings: Seq[String]): (Seq[Boolean], Map[(Boolean, Boolean, Boolean, Boolean, Boolean), Boolean]) = {
    (stringToPlants(strings.head.split(": ")(1)), stringsToRules(strings.drop(2)))
  }
}

case class Pot(value: Boolean, number: Int, var left: Pot = null, var right: Pot = null) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: Pot => value == other.value && number == other.number && right == other.right
      case _          => false
    }
  }
}

object Pot {

  def fromSeq(seq: Seq[Boolean], firstNumber: Integer): (Pot, Pot) = {
    val nodes: Seq[Pot] = seq.zipWithIndex.map((p: (Boolean, Int)) => Pot(p._1, p._2 + firstNumber))
    (0 until nodes.size - 1).foreach { i: Int =>
      nodes(i).right = nodes(i + 1)
      nodes(i + 1).left = nodes(i)
    }
    (nodes.head, nodes.last)
  }

  def toSeq(p: Pot): Seq[(Boolean, Int)] = {
    if (p == null) Seq.empty
    else (p.value, p.number) +: toSeq(p.right)
  }
}
