package day15

class Combat(strings: Seq[String]) {
  Elves.enemy = Goblins
  Goblins.enemy = Elves

  val nodesGrid: Seq[Seq[Option[Node]]] = for (y <- strings.indices) yield {
    for (x <- strings(y).indices) yield {
      if (Set('.', 'E', 'G').contains(strings(y)(x))) Some(Node((x, y), new Neighbors(), None))
      else None
    }
  }
  nodesGrid.foreach((y: Seq[Option[Node]]) =>
    y.foreach { n: Option[Node] =>
      n match {
        case Some(Node(l, neighbors, _)) => assignNeighbors(l, neighbors)
        case _                           =>
      }
      def assignNeighbors(location: (Int, Int), neighbors: Neighbors): Unit = {
        val up: Option[Node] = nodesGrid(location._2 - 1)(location._1)
        if (up.isDefined) neighbors.up = up.get
        val down: Option[Node] = nodesGrid(location._2 + 1)(location._1)
        if (down.isDefined) neighbors.down = down.get
        val left: Option[Node] = nodesGrid(location._2)(location._1 - 1)
        if (left.isDefined) neighbors.left = left.get
        val right: Option[Node] = nodesGrid(location._2)(location._1 + 1)
        if (right.isDefined) neighbors.right = right.get
      }
  })
  strings.indices.foreach { y: Int =>
    (0 until strings(y).length).foreach { x: Int =>
      if (strings(y)(x) == 'E') {
        val newElf = new Elf(nodesGrid(y)(x).get, Elves)
        Elves.members = Elves.members + newElf
        newElf.occupies.occupier = Some(newElf)
      }
      else if (strings(y)(x) == 'G') {
        val newGoblin = new Goblin(nodesGrid(y)(x).get, Goblins)
        Goblins.members = Goblins.members + newGoblin
        newGoblin.occupies.occupier = Some(newGoblin)
      }
    }
  }
  val allNodes: Set[Node] = nodesGrid.flatten.flatten.toSet

  def round(withAttack: Boolean = true): Boolean = {
    var fullRound: Boolean = true
    (Elves.members ++ Goblins.members).filter(_.alive).toSeq.sortBy(_.occupies.location.swap).foreach { m: Member =>
      if (m.alive) {
        if (!m.team.enemy.members.exists(_.alive)) fullRound = false
        m.move()
        if (withAttack) m.attack()
      }
    }
    fullRound
  }

  def wageWar(exitCondition: () => Boolean = () => { false }): (Int, Int) = {
    var rounds = 0
    while (Elves.members.exists(_.alive) && Goblins.members.exists(_.alive) && !exitCondition()) {
      if (round()) rounds += 1
    }
    (rounds, (Elves.members ++ Goblins.members).filter(_.alive).toSeq.map(_.hp).sum.toInt)
  }

  trait Member {
    var hp: Short = 200
    var occupies: Node
    var team: Team
    def attackPower: Short = team.attackPower
    def alive: Boolean = hp > 0

    def canAttack: Boolean = {
      alive && potentialAttackTargets.nonEmpty
    }

    def potentialAttackTargets: Set[Member] = {
      occupies.neighbors.toSet.map(_.occupier).collect { case Some(m) if m.team != team && m.alive => m }
    }

    def canMove: Boolean = { alive && !canAttack }

    def move(): Unit = {
      val aliveEnemyNeighborNodes: Set[Node] =
        team.enemy.members.filter(_.alive).map(_.occupies).flatMap(_.neighbors.toSet)
      val reachableAliveEnemyNeighborNodes: Set[Node] =
        aliveEnemyNeighborNodes.filter((n: Node) => occupies.pathExists(n))
      if (canMove && reachableAliveEnemyNeighborNodes.nonEmpty) {
        val candidateDestinations: Set[Node] =
          reachableAliveEnemyNeighborNodes
            .map((n: Node) => (occupies.nearestNeighborsOfWithDistance(n)._1 + 1, n))
            .groupMap(_._1)(_._2)
            .minBy(_._1)
            ._2
        if (candidateDestinations.nonEmpty) {
          val destination: Node = candidateDestinations.minBy(_.location.swap)
          val candidateNeighbors: Set[Node] = destination.nearestNeighborsOfWithDistance(occupies)._2
          if (candidateNeighbors.nonEmpty) {
            occupies.occupier = None
            occupies = candidateNeighbors.minBy(_.location.swap)
            occupies.occupier = Some(this)
          }
        }
      }
    }

    def attack(): Unit = {
      val targets: Set[Member] = potentialAttackTargets
      if (alive && targets.nonEmpty) {
        val target: Member = targets.groupBy(_.hp).minBy(_._1)._2.minBy(_.occupies.location.swap)
        target.hp = (target.hp - attackPower).toShort
        if (!target.alive) {
          target.occupies.occupier = None
        }
      }
    }
  }
  class Elf(var occupies: Node, var team: Team) extends Member
  class Goblin(var occupies: Node, var team: Team) extends Member

  trait Team {
    var members: Set[Member] = Set.empty
    var attackPower: Short = 3
    var enemy: Team = _
  }
  object Elves extends Team
  object Goblins extends Team

  case class Node(location: (Int, Int), neighbors: Neighbors, var occupier: Option[Member]) {

    def nearestNeighborsOfWithDistance(to: Node): (Int, Set[Node]) = {
      var unvisited: Map[Node, Int] = allNodes.map((_, Int.MaxValue)).toMap + (this -> 0)
      var visited: Set[(Node, Int)] = Set.empty
      var curNode: (Node, Int) = unvisited.minBy(_._2)
      while (curNode._1 != to && curNode._2 < Int.MaxValue) {
        val curUnvisitedAvailableNeighbors: Map[Node, Int] =
          unvisited
            .filter((n: (Node, Int)) => curNode._1.neighbors.toSet.contains(n._1) && n._1.occupier.isEmpty)
            .map((n: (Node, Int)) => (n._1, curNode._2 + 1))
        unvisited = unvisited.removed(curNode._1) ++ curUnvisitedAvailableNeighbors
        visited = visited + curNode
        curNode = unvisited.minBy(_._2)
      }
      visited.filter((n: (Node, Int)) => to.neighbors.toSet.contains(n._1)).groupMap(_._2)(_._1).minBy(_._1)
    }

    def pathExists(to: Node): Boolean = {
      var visited: Set[Node] = Set.empty
      var toVisit: Set[Node] = Set(this)
      var found: Boolean = false
      while (toVisit.nonEmpty && !found) {
        if (toVisit.contains(to)) found = true
        else {
          visited = visited ++ toVisit
          toVisit = toVisit
            .flatMap((n: Node) => n.neighbors.toSet.filter((n: Node) => n.occupier.isEmpty))
            .diff(visited)
        }
      }
      found
    }
  }

  class Neighbors(var up: Node = null, var down: Node = null, var left: Node = null, var right: Node = null) {

    def toSet: Set[Node] = {
      var s: Set[Node] = Set.empty
      def add(n: Node): Unit = {
        if (n != null) s = s + n
      }
      add(up); add(left); add(right); add(down)
      s
    }
  }
}

object Combat {

  def outcomeOfBattleWithLowestElfAttackPower(strings: Seq[String],
                                              minPower: Short = 4,
                                              maxPower: Short = 200): Option[(Int, Int, Short)] = {
    if (maxPower - minPower == 1) {
      println(minPower + "," + maxPower)
      val outComes: Set[(Int, Int, Short)] = Set(
        outcomeOfBattleWithLowestElfAttackPower(strings, minPower, minPower),
        outcomeOfBattleWithLowestElfAttackPower(strings, maxPower, maxPower)).flatten
      if (outComes.isEmpty) None
      else Some(outComes.minBy(_._3))
    }
    else {
      val attackPower: Short = ((minPower + maxPower) / 2).toShort
      println(minPower + "," + attackPower + "," + maxPower)
      val c = new Combat(strings)
      val origElfCount: Int = c.Elves.members.size
      c.Elves.attackPower = attackPower
      val thisOutcome: Option[(Int, Int, Short)] = Some(c.wageWar(() => c.Elves.members.exists(!_.alive)) match {
        case (r, hp) => (r, hp, attackPower)
      })
      if (c.Elves.members.count(_.alive) < origElfCount) {
        if (attackPower < maxPower) {
          outcomeOfBattleWithLowestElfAttackPower(strings, (attackPower + 1).toShort, maxPower)
        }
        else None
      }
      else if (attackPower > minPower) {
        val otherOutcome: Option[(Int, Int, Short)] =
          outcomeOfBattleWithLowestElfAttackPower(strings, minPower, (attackPower - 1).toShort)
        if (otherOutcome.isDefined && otherOutcome.get._3 < attackPower) otherOutcome
        else thisOutcome
      }
      else thisOutcome
    }
  }
}
