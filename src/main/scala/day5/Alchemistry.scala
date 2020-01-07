package day5

object Alchemistry {

  def areReactingUnits(x: Char, y: Char): Boolean = {
    x != y && x.toLower == y.toLower
  }

  def findUnitStartingWith(curPolymer: String, startIndex: Int): Int = {
    var index: Int = -1
    if (curPolymer.length >= 2) {
      val indices: Iterator[Int] = Range(startIndex max 0, curPolymer.length - 1).iterator
      while (index == -1 && indices.hasNext) {
        val i: Int = indices.next()
        if (areReactingUnits(curPolymer.charAt(i), curPolymer.charAt(i + 1))) index = i
      }
    }
    index
  }

  def reduce(polymer: String): String = {
    var curPolymer: String = polymer
    var unitIndex: Int = findUnitStartingWith(curPolymer, 0)
    while (unitIndex > -1) {
      curPolymer = curPolymer.splitAt(unitIndex)._1 + curPolymer.splitAt(unitIndex + 2)._2
      unitIndex = findUnitStartingWith(curPolymer, unitIndex - 1)
    }
    curPolymer
  }

  def findSmallestReductionRemovingOneProblem(polymer: String): (Char, String) = {
    val reductions: Seq[(Char, String)] =
      for (c <- 'a' to 'z') yield (c, reduce(polymer.replaceAll("[" + c + c.toUpper + "]", "")))
    reductions.minBy(_._2.length)
  }
}
