package day12

class HallOfPlants(var plants: Seq[Boolean], val rules: Map[(Boolean, Boolean, Boolean, Boolean, Boolean), Boolean]) {
  var idxOfCentralPot: Int = 0

  def runGenerations(gens: Int): Unit = {
    val buffer: Seq[Boolean] = (1 to (gens + 2)).map(_ < 0)
    plants = buffer ++ plants ++ buffer
    idxOfCentralPot += gens + 2
    val staticPlantIdxs = Set(0, 1, plants.size - 2, plants.size - 1)
    (1 to gens).foreach { _: Int =>
      plants = for (x <- plants.indices) yield {
        if (staticPlantIdxs.contains(x)) plants(x)
        else rules.getOrElse((plants(x - 2), plants(x - 1), plants(x), plants(x + 1), plants(x + 2)), false)
      }
    }
    idxOfCentralPot -= plants.indexWhere(_ == true)
    plants = plants.dropWhile(_ == false)
    plants = plants.take(plants.lastIndexWhere(_ == true) + 1)
  }

  def mkString: String = {
    plants.map(if (_) '#' else '.').mkString
  }

  def numbersOfPotsWithPlants: Seq[Int] =
    plants.zipWithIndex.filter(_._1 == true).map((p: (Boolean, Int)) => p._2 - idxOfCentralPot)
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
