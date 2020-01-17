package day20

import scala.collection.mutable

object Mapper {
  def distanceToFarthestRoom(regEx: String): Int = {
    visitedLocationDistances(regEx).max._2
  }

  def visitedLocationDistances(regEx: String): LocationOrganizer = {
    type LocationDistance = (Location, Int)
    var curLocationDist: LocationDistance = (Location(0, 0), 0)
    var visitedLocationDistances: LocationOrganizer = LocationOrganizer(Map(curLocationDist))
    val branchPoints: mutable.Stack[LocationDistance] = mutable.Stack.empty
    var curRegExPosition = 1
    while (regEx(curRegExPosition) != '$') {
      regEx(curRegExPosition) match {
        case '(' => branchPoints.push(curLocationDist)
        case '|' => curLocationDist = branchPoints.top
        case ')' => curLocationDist = branchPoints.pop()
        case c =>
          val newLocationDist: LocationDistance = (curLocationDist._1(c), curLocationDist._2 + 1)
          visitedLocationDistances = visitedLocationDistances.updated(newLocationDist)
          curLocationDist = newLocationDist
      }
      curRegExPosition += 1
    }
    visitedLocationDistances
  }
}

case class Location(x: Int, y: Int) {
  def apply(direction: Char): Location = {
    direction match {
      case 'N' => Location(x, y + 1)
      case 'S' => Location(x, y - 1)
      case 'E' => Location(x + 1, y)
      case 'W' => Location(x - 1, y)
    }
  }
}

case class LocationOrganizer(locations: Map[Location, Int]) {
  def +(elem: (Location, Int)): LocationOrganizer = updated(elem)

  def max: (Location, Int) = {
    locations.maxBy(_._2)
  }

  def updated(elem: (Location, Int)): LocationOrganizer = {
    LocationOrganizer(locations.updatedWith(elem._1)((v: Option[Int]) => Some(v.getOrElse(Int.MaxValue) min elem._2)))
  }
}

object LocationOrganizer {
  def apply(elems: Map[Location, Int]): LocationOrganizer = new LocationOrganizer(elems)
}
