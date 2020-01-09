package day9

import org.scalatest.flatspec.AnyFlatSpec

class MarbleGameTest extends AnyFlatSpec {
  behavior of "play"
  it should "handle examples" in {
    assert(new MarbleGame(playerCount = 1, lastMarbleValue = 1).play == 0)
    assert(new MarbleGame(playerCount = 1, lastMarbleValue = 23).play == 32)
    assert(new MarbleGame(playerCount = 7, lastMarbleValue = 25).play == 32)
    assert(new MarbleGame(playerCount = 10, lastMarbleValue = 1618).play == 8317)
    assert(new MarbleGame(playerCount = 13, lastMarbleValue = 7999).play == 146373)
    assert(new MarbleGame(playerCount = 17, lastMarbleValue = 1104).play == 2764)
    assert(new MarbleGame(playerCount = 21, lastMarbleValue = 6111).play == 54718)
    assert(new MarbleGame(playerCount = 30, lastMarbleValue = 5807).play == 37305)
  }
  it should "solve puzzle 1" in {
    assert(new MarbleGame(playerCount = 446, lastMarbleValue = 71522).play == 390592)
  }
  it should "solve puzzle 2" in {
    assert(new MarbleGame(playerCount = 446, lastMarbleValue = 7152200).play == 3277920293L)
  }
}
