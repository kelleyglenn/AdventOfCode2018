package day2

object IMS {
  def hasCharsTwiceOrThrice(s: String): (Boolean, Boolean) = {
    val charCounts: Map[Char, Int] =
      s.toSet.map((c: Char) => (c, s.count(_ == c))).toMap
    (charCounts.values.exists(_ == 2), charCounts.values.exists(_ == 3))
  }

  def checksum(boxIDs: Seq[String]): Int = {
    val matches: Seq[(Boolean, Boolean)] = boxIDs.map(hasCharsTwiceOrThrice)
    matches.count(_._1) * matches.count(_._2)
  }

  def charactersInCommon(first: String, second: String): String = {
    assert(first.length == second.length)
    first.toSeq
      .zip(second.toSeq)
      .collect { case (x, y) if x == y => x }
      .mkString
  }

  def mostCharacters(x: String, y: String): String = {
    if (y.length > x.length) y else x
  }

  def mostCharactersInCommonWith(boxID: String, boxIDs: Seq[String]): String = {
    if (boxIDs.isEmpty) ""
    else
      mostCharacters(
        charactersInCommon(boxID, boxIDs.head),
        mostCharactersInCommonWith(boxID, boxIDs.tail)
      )
  }

  def mostCharactersInCommonBetweenAnyTwo(boxIDs: Seq[String]): String = {
    if (boxIDs.isEmpty) ""
    else {
      mostCharacters(
        mostCharactersInCommonWith(boxIDs.head, boxIDs.tail),
        mostCharactersInCommonBetweenAnyTwo(boxIDs.tail)
      )
    }
  }
}
