package day22

import java.text.SimpleDateFormat
import java.util.Calendar

object CaveSystem {
  def riskLevel(depth: Int, targetX: Int, targetY: Int): Int = {
    val riskLevels: Seq[Seq[Int]] = createRiskLevels(depth, targetX, targetY, 0, 0)
    riskLevels.map(_.sum).sum
  }

  def shortestPathInMinutes(depth: Int, targetX: Int, targetY: Int): Int = {
    val allowedTools: Map[Int, Set[Tool.Value]] =
      Map(0 -> Set(Tool.ClimbingGear, Tool.Torch),
        1 -> Set(Tool.ClimbingGear, Tool.Neither),
        2 -> Set(Tool.Torch, Tool.Neither))

    def fromTo(from: (Int, (Int, Set[Tool.Value])), to: Int): (Int, Set[Tool.Value]) = {
      (from._1, to, from._2._1, from._2._2) match {
        case (fromType, toType, dist, fromTools) if fromType == toType => (dist + 1, fromTools)
        case (_, toType, dist, fromTools) if allowedTools(toType).intersect(fromTools).nonEmpty => (dist + 1, allowedTools(toType).intersect(fromTools))
        case (fromType, toType, dist, _) => (dist + 8, allowedTools(toType).intersect(allowedTools(fromType)))
      }
    }

    def merge(one: (Int, Set[Tool.Value]), other: (Int, Set[Tool.Value])): (Int, Set[Tool.Value]) = {
      if (one._1 == other._1) (one._1, one._2 ++ other._2)
      else if (one._1 < other._1) one
      else other
    }

    val dateFormat = new SimpleDateFormat("HH:mm:ss")
    val neighborsDiff: Set[(Int, Int)] = Set((0, -1), (0, 1), (-1, 0), (1, 0))
    val riskLevels: Seq[Seq[Int]] = createRiskLevels(depth, targetX, targetY, (targetY * 4).toShort, (targetX * 4).toShort)
    var unvisited: Map[(Int, Int), (Int, Set[Tool.Value])] = riskLevels.zipWithIndex.flatMap {
      case (row, y) => row.zipWithIndex.map {
        case (_, x) => ((x, y), (Int.MaxValue, Set.empty.asInstanceOf[Set[Tool.Value]]))
      }
    }.toMap
    var visited: Int = 0
    var prelims: Seq[((Int, Int), (Int, Set[Tool.Value]))] = Seq((0, 0) -> (0, Set(Tool.Torch)))
    var curNode: ((Int, Int), (Int, Set[Tool.Value])) = prelims.head
    prelims = prelims.tail
    while (curNode._1 != (targetX, targetY)) {
      val allUnvisited: Map[(Int, Int), (Int, Set[Tool.Value])] = unvisited ++ prelims.toMap
      val neighbors: Set[(Int, Int)] = neighborsDiff.map { case (xDiff, yDiff) => (curNode._1._1 + xDiff, curNode._1._2 + yDiff) }
      val curUnvisitedNeighbors: Set[((Int, Int), (Int, Set[Tool.Value]))] =
        neighbors.collect { case i if allUnvisited.contains(i) => (i, allUnvisited(i)) }
      val newPrelims: Set[((Int, Int), (Int, Set[Tool.Value]))] =
        curUnvisitedNeighbors.map { n: ((Int, Int), (Int, Set[Tool.Value])) =>
          var newDistTools: (Int, Set[Tool.Value]) = fromTo((riskLevels(curNode._1._2)(curNode._1._1), curNode._2), riskLevels(n._1._2)(n._1._1))
          if (n._1 == (targetX, targetY) && !newDistTools._2.contains(Tool.Torch))
            newDistTools = (newDistTools._1 + 7, Set(Tool.Torch))
          (n._1, merge(newDistTools, n._2))
        }
      val newPrelimLocations: Set[(Int, Int)] = newPrelims.map(_._1)
      unvisited = unvisited.removedAll(curUnvisitedNeighbors.map(_._1))
      prelims = (prelims.filterNot((p: ((Int, Int), (Int, Set[Tool.Value]))) => newPrelimLocations.contains(p._1)) ++ newPrelims).sortBy(_._2._1)
      visited += 1
      curNode = prelims.head
      prelims = prelims.tail
      if (visited % 3000 == 0)
        println(dateFormat.format(Calendar.getInstance().getTime) + ", visited: " + visited + ", prelims: " + prelims.size + ", unvisited: " + unvisited.size)
    }
    curNode._2._1
  }

  def createRiskLevels(depth: Int, targetX: Int, targetY: Int, padX: Short, padY: Short): Seq[Seq[Int]] = {
    val geologicIndices: Array[Array[Int]] = (for (_ <- 0 to targetY + padY) yield {
      new Array[Int](targetX + 1 + padX)
    }).toArray
    val erosionLevels: Array[Array[Int]] = geologicIndices.map(_.clone())
    val riskLevels: Array[Array[Int]] = geologicIndices.map(_.clone())
    geologicIndices.indices.foreach { y: Int =>
      geologicIndices.head.indices.foreach { x: Int =>
        geologicIndices(y)(x) = (x, y) match {
          case (0, 0) => 0
          case (_, _) if x == targetX && y == targetY => 0
          case (_, 0) => x * 16807
          case (0, _) => y * 48271
          case (_, _) => erosionLevels(y)(x - 1) * erosionLevels(y - 1)(x)
        }
        erosionLevels(y)(x) = (geologicIndices(y)(x) + depth) % 20183
        riskLevels(y)(x) = erosionLevels(y)(x) % 3
      }
    }
    riskLevels.map(_.toSeq).toSeq
  }

  def riskLevelsToStrings(riskLevels: Seq[Seq[Int]]): Seq[String] = {
    riskLevels.map(_.map {
      case 0 => '.'
      case 1 => '='
      case 2 => '|'
    }.mkString)
  }

  object Tool extends Enumeration {
    type Tool = Value
    val ClimbingGear, Torch, Neither = Value
  }

}
