package day15

import org.scalatest.flatspec.AnyFlatSpec
import scala.io.{BufferedSource, Source}

class CombatTest extends AnyFlatSpec {
  behavior of "constructor"
  it should "handle example 1" in new SetupData("example1") {
    val c = new Combat(lines)
    assert(c.allNodes.size == 15)
    assert(c.Elves.members.size == 4)
    assert(c.Goblins.members.size == 3)
    assert(!c.Elves.members.exists(_.occupies == null))
    assert(!c.Goblins.members.exists(_.occupies == null))
    assert(c.Elves.members.exists(_.occupies.location == (4, 1)))
    assert(c.Goblins.members.exists(_.occupies.location == (3, 2)))
    assert(!c.Goblins.members.exists(_.occupies.location == (4, 1)))
    assert(!c.Elves.members.exists(_.occupies.location == (3, 2)))
    assert(c.Goblins.members.find(_.occupies.location == (2, 1)).get.occupies.neighbors.up == null)
  }

  behavior of "pathExists"
  it should "handle example 1" in new SetupData("example1") {
    val c = new Combat(lines)
    val topLeft: c.Node = c.nodesGrid(1)(1).get
    val bottomRight: c.Node = c.nodesGrid(3)(5).get
    val topG: c.Node = c.nodesGrid(1)(2).get
    val middleG: c.Node = c.nodesGrid(2)(3).get
    assert(topLeft.pathExists(topLeft))
    assert(!topLeft.pathExists(topG))
    assert(topG.pathExists(topLeft))
    assert(!topLeft.pathExists(middleG))
    assert(!topLeft.pathExists(bottomRight))
  }

  behavior of "move"
  it should "handle example 1" in new SetupData("example1") {
    val c = new Combat(lines)
    val topG: c.Member = c.nodesGrid(1)(2).get.occupier.get
    val centerG: c.Member = c.nodesGrid(2)(3).get.occupier.get
    val rightE: c.Member = c.nodesGrid(2)(5).get.occupier.get
    topG.move()
    assert(topG.occupies.location == (1, 1))
    topG.move() // can't move
    assert(topG.occupies.location == (1, 1))
    centerG.move()
    assert(centerG.occupies.location == (3, 1))
    centerG.move()
    assert(centerG.occupies.location == (3, 1))
    rightE.move()
    assert(rightE.occupies.location == (4, 2))
    rightE.move()
    assert(rightE.occupies.location == (3, 2))
    rightE.move()
    assert(rightE.occupies.location == (3, 2))
  }
  it should "handle example 2" in new SetupData("example2") {
    val c = new Combat(lines)
    val topG: c.Member = c.nodesGrid(1)(4).get.occupier.get
    val rightG: c.Member = c.nodesGrid(3)(5).get.occupier.get
    rightG.move() // can't move
    assert(rightG.occupies.location == (5, 3))
    topG.move()
    assert(topG.occupies.location == (3, 1))
    rightG.move() // still can't move
    assert(rightG.occupies.location == (5, 3))
    topG.move()
    assert(topG.occupies.location == (2, 1))
    rightG.move() // now can move
    assert(rightG.occupies.location == (5, 2))
  }

  behavior of "round without attack"
  it should "handle example 3" in new SetupData("example3") {
    val c = new Combat(lines)
    val e: c.Member = c.nodesGrid(4)(4).get.occupier.get
    c.round(false)
    assert(e.occupies.location == (4, 3))
    assert(e.occupies.neighbors.up.occupier.isDefined)
    assert(e.occupies.neighbors.left.occupier.isEmpty)
    assert(e.occupies.neighbors.right.occupier.isEmpty)
    assert(e.occupies.neighbors.down.occupier.isEmpty)
    c.round(false)
    assert(e.occupies.location == (4, 3))
    assert(e.occupies.neighbors.up.occupier.isDefined)
    assert(e.occupies.neighbors.left.occupier.isEmpty)
    assert(e.occupies.neighbors.right.occupier.isEmpty)
    assert(e.occupies.neighbors.down.occupier.isEmpty)
    c.round(false)
    assert(e.occupies.location == (4, 3))
    assert(e.occupies.neighbors.up.occupier.isDefined)
    assert(e.occupies.neighbors.left.occupier.isDefined)
    assert(e.occupies.neighbors.right.occupier.isDefined)
    assert(e.occupies.neighbors.down.occupier.isDefined)
    assert(e.occupies.neighbors.up.neighbors.left.occupier.isDefined)
    assert(e.occupies.neighbors.up.neighbors.right.occupier.isDefined)
    assert(e.occupies.neighbors.down.neighbors.left.neighbors.left.neighbors.left.occupier.isDefined)
    assert(e.occupies.neighbors.down.neighbors.down.neighbors.right.neighbors.right.neighbors.right.occupier.isDefined)
  }

  behavior of "round with attack"
  it should "handle example 4" in new SetupData("example4") {
    val c = new Combat(lines)
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 400)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 800)
    c.round() // round 1
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 394)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 794)
    c.round() // round 2
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 382)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 788)
    (1 to 21).foreach((_: Int) => c.round()) // round 23
    assert(c.Elves.members.count(_.alive) == 1)
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 131)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 662)
    assert(c.Elves.members.filter(_.alive).map(_.occupies.location) == Set((5, 4)))
    assert(c.Goblins.members.filter(_.alive).map(_.occupies.location) == Set((4, 1), (3, 2), (5, 3), (5, 2)))
    c.round() // round 24
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 128)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 659)
    assert(c.Elves.members.filter(_.alive).map(_.occupies.location) == Set((5, 4)))
    assert(c.Goblins.members.filter(_.alive).map(_.occupies.location) == Set((3, 1), (3, 3), (5, 3), (4, 2)))
    c.round() // round 25
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 125)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 656)
    (1 to 22).foreach((_: Int) => c.round()) // round 47
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 0)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 590)
  }
  it should "handle example 5" in new SetupData("example5") {
    val c = new Combat(lines)
    val topG: c.Member = c.nodesGrid(1)(1).get.occupier.get
    val bottomG: c.Member = c.nodesGrid(3)(1).get.occupier.get
    val topE: c.Member = c.nodesGrid(1)(5).get.occupier.get
    val leftE: c.Member = c.nodesGrid(2)(1).get.occupier.get
    val middleE: c.Member = c.nodesGrid(2)(3).get.occupier.get
    val rightMiddleE: c.Member = c.nodesGrid(2)(5).get.occupier.get
    val rightBottomE: c.Member = c.nodesGrid(4)(5).get.occupier.get
    val bottomE: c.Member = c.nodesGrid(5)(4).get.occupier.get
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 1200)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 400)
    c.round() // round 1, 2G vs 1E
    assert(c.Goblins.members.count(_.canAttack) == 2)
    assert(c.Elves.members.count(_.canAttack) == 1)
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 1194)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 397)
    c.round() // round 2, 2G vs 2E
    assert(c.Goblins.members.count(_.canAttack) == 2)
    assert(c.Elves.members.count(_.canAttack) == 2)
    assert(middleE.occupies.location == (2, 1))
    assert(bottomE.occupies.location == (3, 4))
    assert(rightBottomE.occupies.location == (5, 4))
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 1188)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 391)
    c.round() // round 3, 2G vs 2E
    assert(middleE.occupies.location == (2, 1))
    assert(bottomE.occupies.location == (2, 4))
    assert(rightBottomE.occupies.location == (5, 5))
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 1182)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 385)
    c.round() // round 4, 2G vs 3E
    assert(c.Goblins.members.count(_.canAttack) == 2)
    assert(c.Elves.members.count(_.canAttack) == 3)
    assert(middleE.occupies.location == (2, 1))
    assert(bottomE.occupies.location == (2, 3))
    assert(rightBottomE.occupies.location == (4, 5))
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 1176)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 376)
    c.round() // round 5, 2G vs 3E
    assert(middleE.occupies.location == (2, 1))
    assert(bottomE.occupies.location == (2, 3))
    assert(rightBottomE.occupies.location == (3, 5))
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 1170)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 367)
    c.round() // round 6, 2G vs 3E
    assert(middleE.occupies.location == (2, 1))
    assert(bottomE.occupies.location == (2, 3))
    assert(rightBottomE.occupies.location == (3, 4))
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 1164)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 358)
    c.round() // round 7, 2G vs 3E
    assert(middleE.occupies.location == (2, 1))
    assert(bottomE.occupies.location == (2, 3))
    assert(rightBottomE.occupies.location == (2, 4))
    assert(rightMiddleE.occupies.location == (5, 3))
    assert(c.Goblins.members.count(_.canAttack) == 2)
    assert(c.Elves.members.count(_.canAttack) == 3)
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 1158)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 349)
    c.round() // round 8, 2G vs 4E
    assert(middleE.occupies.location == (2, 1))
    assert(bottomE.occupies.location == (2, 3))
    assert(rightBottomE.occupies.location == (1, 4))
    assert(rightMiddleE.occupies.location == (5, 4))
    assert(c.Goblins.members.count(_.canAttack) == 2)
    assert(c.Elves.members.count(_.canAttack) == 4)
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 1152)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 337)
    (1 to 25).foreach((_: Int) => c.round()) // round 33, 2G vs 4E
    assert(topG.hp == 5)
    assert(bottomG.hp == 32)
    assert(leftE.hp == 2)
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 1002)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 37)
    c.round() // round 34, 2G vs 3E
    assert(topG.hp == 2)
    assert(bottomG.hp == 26)
    assert(leftE.hp == -1)
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 997)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 28)
    c.round() // round 35, 2G vs 3E
    assert(topG.hp == -1)
    assert(bottomG.hp == 20)
    assert(middleE.hp == 197)
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 991)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 20)
    c.round() // round 36, 1G vs 3E
    assert(bottomG.hp == 14)
    assert(middleE.hp == 197)
    assert(rightBottomE.occupies.location == (1, 4))
    assert(c.Elves.members.filter(_.alive).toSeq.map(_.hp).sum == 988)
    assert(c.Goblins.members.filter(_.alive).toSeq.map(_.hp).sum == 14)
  }

  behavior of "wageWar"
  it should "handle example 4" in new SetupData("example4") {
    val c = new Combat(lines)
    assert(c.wageWar == (47, 590))
  }
  it should "handle example 5" in new SetupData("example5") {
    val c = new Combat(lines)
    assert(c.wageWar == (37, 982))
  }
  it should "handle example 6" in new SetupData("example6") {
    val c = new Combat(lines)
    assert(c.wageWar == (46, 859))
  }
  it should "solve the puzzle" in new SetupData("input") {
    val c = new Combat(lines)
    assert(c.wageWar == (85, 2674))
  }

  class SetupData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day15/" + name + ".txt"))
    val lines: Seq[String] = bufferedSource.getLines.toSeq
  }

}
