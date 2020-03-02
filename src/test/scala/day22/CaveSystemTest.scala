package day22

import org.scalatest.flatspec.AnyFlatSpec

class CaveSystemTest extends AnyFlatSpec {
  behavior of "riskLevel"
  it should "handle the example" in {
    assert(CaveSystem.riskLevel(510, 10, 10) == 114)
  }
  it should "solve the puzzle" in {
    assert(CaveSystem.riskLevel(11109, 9, 731) == 7299)
  }

  behavior of "riskLevels"
  it should "handle the example" in {
    val expected = Seq(
      ".=.|=.|.|=.|=|=.",
      ".|=|=|||..|.=...",
      ".==|....||=..|==",
      "=.|....|.==.|==.",
      "=|..==...=.|==..",
      "=||.=.=||=|=..|=",
      "|.=.===|||..=..|",
      "|..==||=.|==|===",
      ".=..===..=|.|||.",
      ".======|||=|=.|=",
      ".===|=|===.===||",
      "=|||...|==..|=.|",
      "=.=|=.=..=.||==|",
      "||=|=...|==.=|==",
      "|=.=||===.|||===",
      "||.|==.|.|.||=||"
    )
    assert(CaveSystem.riskLevelsToStrings(CaveSystem.createRiskLevels(510, 10, 10, 5, 5)) == expected)
  }

  behavior of "shortestPathInMinutes"
  it should "handle a custom example" in {
    val riskLevels: Seq[String] = CaveSystem.riskLevelsToStrings(CaveSystem.createRiskLevels(11109, 9, 9, 36, 36))
    riskLevels.foreach(println(_))
    assert(CaveSystem.shortestPathInMinutes(11109, 9, 9) == 45)
  }
  it should "handle the example" in {
    assert(CaveSystem.shortestPathInMinutes(510, 10, 10) == 45)
  }
  //  it should "solve the puzzle" in {
  //    assert(CaveSystem.shortestPathInMinutes(11109, 9, 731) == 45)
  //    //1020 too high
  //  }
}
