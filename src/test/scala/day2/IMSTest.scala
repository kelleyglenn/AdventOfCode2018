package day2

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class IMSTest extends AnyFlatSpec {
  behavior of "hasCharsTwiceOrThrice"
  it should "handle examples" in {
    assert(IMS.hasCharsTwiceOrThrice("abcdef") == (false, false))
    assert(IMS.hasCharsTwiceOrThrice("bababc") == (true, true))
    assert(IMS.hasCharsTwiceOrThrice("abbcde") == (true, false))
    assert(IMS.hasCharsTwiceOrThrice("abcccd") == (false, true))
    assert(IMS.hasCharsTwiceOrThrice("aabcdd") == (true, false))
    assert(IMS.hasCharsTwiceOrThrice("abcdee") == (true, false))
    assert(IMS.hasCharsTwiceOrThrice("ababab") == (false, true))
  }

  behavior of "checksum"
  it should "handle examples" in {
    val boxIDs =
      Seq("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
    assert(IMS.checksum(boxIDs) == 12)
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(IMS.checksum(boxIDs) == 6944)
  }

  behavior of "charactersInCommon"
  it should "handle examples" in {
    assert(IMS.charactersInCommon("abcde", "abcde") == "abcde")
    assert(IMS.charactersInCommon("abcde", "fghij") == "")
    assert(IMS.charactersInCommon("abcde", "axcye") == "ace")
    assert(IMS.charactersInCommon("fghij", "fguij") == "fgij")
  }

  behavior of "mostCharactersInCommonWith"
  it should "handle examples" in {
    assert(
      IMS.mostCharactersInCommonWith("fghij", Seq("abcde", "fguij", "axcye")) == "fgij"
    )
  }

  behavior of "mostCharactersInCommonBetweenAnyTwo"
  it should "handle examples" in {
    assert(
      IMS.mostCharactersInCommonBetweenAnyTwo(
        Seq("abcde", "fguij", "axcye", "fghij")
      ) == "fgij"
    )
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(
      IMS.mostCharactersInCommonBetweenAnyTwo(boxIDs) == "srijafjzloguvlntqmphenbkd"
    )
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource =
      Source.fromURL(getClass.getResource("/day2/" + name + ".txt"))
    val boxIDs: Seq[String] =
      bufferedSource.getLines.toSeq
  }
}
