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

  def wageWar: (Int, Int) = {
    var rounds = 0
    while (Elves.members.exists(_.alive) && Goblins.members.exists(_.alive)) {
      if (round()) rounds += 1
    }
    (rounds, (Elves.members ++ Goblins.members).filter(_.alive).toSeq.map(_.hp).sum.toInt)
  }

  trait Member {
    var hp: Short = 200
    var attackPower: Short = 3
    var occupies: Node
    var team: Team
    def alive: Boolean = hp > 0

    def canAttack: Boolean = {
      alive && potentialTargets.nonEmpty
    }

    def potentialTargets: Set[Member] = {
      occupies.neighbors.toSet.map(_.occupier).collect { case Some(m) if m.team != team && m.alive => m }
    }

    def nodesAdjacentToTargets: Set[Node] =
      team.enemy.members
        .filter(_.alive)
        .flatMap(_.occupies.neighbors.toSeq)
        .filter((n: Node) => n.occupier.isEmpty)

    def nodesReachable: Set[Node] = nodesAdjacentToTargets.filter((n: Node) => occupies.pathExists(n))

    def canMove: Boolean = { alive && !canAttack }

    def move(): Unit = {
      if (canMove) {
        val candidateNeighbors: Map[Int, Set[Set[Node]]] =
          nodesReachable.map((n: Node) => n.nearestNeighborsOfWithDistance(occupies)).groupMap(_._1)(_._2)
        if (candidateNeighbors.nonEmpty) {
          occupies.occupier = None
          occupies = candidateNeighbors.minBy(_._1)._2.flatten.minBy(_.location.swap)
          occupies.occupier = Some(this)
        }
      }
    }

    def attack(): Unit = {
      val targets: Set[Member] = potentialTargets
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
            .map((n: (Node, Int)) => (n._1, n._2 min (curNode._2 + 1)))
        // I don't think the "min" is necessary when the distance between neighbors is always 1
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

    def toSeq: Seq[Node] = {
      var s: Seq[Node] = Seq.empty
      def add(n: Node): Unit = {
        if (n != null) s = s :+ n
      }
      add(up); add(left); add(right); add(down)
      s
    }
  }
}
