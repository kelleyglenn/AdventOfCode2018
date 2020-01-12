package day13

class MineTracks(val track: Seq[String]) {
  var locationOfFirstCrash: Option[(Int, Int)] = None
  var carts: Set[Cart] = cartsFromTrack
  var ticks: Int = 0

  def cartsFromTrack: Set[Cart] = {
    var cs: Set[Cart] = Set.empty
    track.indices.foreach { y: Int =>
      track(y).indices.foreach { x: Int =>
        val c: Char = track(y)(x)
        if (Direction.values.map(_.c).contains(c)) cs = cs + Cart((x, y), Direction.values.find(_.c == c).get)
      }
    }
    cs
  }

  def newDirectionTurn(location: (Int, Int),
                       curDirection: Direction.Value,
                       curTurn: Turn.Value): (Direction.Value, Turn.Value) = {
    track(location._2)(location._1) match {
      case '/' =>
        (curDirection match {
          case Direction.U => Direction.R
          case Direction.D => Direction.L
          case Direction.L => Direction.D
          case Direction.R => Direction.U
        }, curTurn)
      case '\\' =>
        (curDirection match {
          case Direction.U => Direction.L
          case Direction.D => Direction.R
          case Direction.L => Direction.U
          case Direction.R => Direction.D
        }, curTurn)
      case '+' =>
        (curTurn.newDir(curDirection), curTurn.nextTurn)
      case _ => (curDirection, curTurn)
    }
  }

  def moveCart(c: Cart): Cart = {
    val newLocation: (Int, Int) = (c.location._1 + c.facing.diff._1, c.location._2 + c.facing.diff._2)
    val newDirTurn: (Direction.Value, Turn.Value) = newDirectionTurn(newLocation, c.facing, c.readiedTurn)
    Cart(newLocation, newDirTurn._1, newDirTurn._2)
  }

  def tick(removeCrashes: Boolean = false): Unit = {
    ticks += 1
    val sortedCarts: Seq[Cart] = carts.toSeq.sortBy(_.location)
    var newCarts: Set[Cart] = Set.empty
    sortedCarts.foreach { c: Cart =>
      carts = carts - c
      newCarts = newCarts + (if (newCarts.map(_.location).contains(c.location)) c else moveCart(c))
      if (locationOfFirstCrash.isEmpty)
        locationOfFirstCrash = (carts ++ newCarts).groupBy(_.location).find(_._2.size > 1).map(_._1)
    }
    carts =
      if (removeCrashes)
        newCarts.filterNot((c: Cart) => newCarts.groupBy(_.location).filter(_._2.size > 1).keySet.contains(c.location))
      else newCarts
  }

  def tickUntilFirstCrash(): (Int, Int) = {
    while (locationOfFirstCrash.isEmpty) tick()
    locationOfFirstCrash.get
  }

  def removeCrashes(): Option[(Int, Int)] = {
    while (carts.size > 1) tick(true)
    carts.find((_: Cart) => true).map(_.location)
  }
}

case class Cart(location: (Int, Int), facing: Direction.Value, readiedTurn: Turn.Value = Turn.L)

object Turn extends Enumeration {
  case class Val(newDir: Direction.Value => Direction.Value, var nextTurn: Turn.Value) extends super.Val
  import scala.language.implicitConversions
  implicit def valueToTurnVal(x: Value): Val = x.asInstanceOf[Val]

  val L: Turn.Value = Val(
    Map(Direction.U -> Direction.L, Direction.D -> Direction.R, Direction.L -> Direction.D, Direction.R -> Direction.U),
    null)
  val S: Turn.Value = Val((d: Direction.Value) => d, null)

  val R: Turn.Value = Val(
    Map(Direction.U -> Direction.R, Direction.D -> Direction.L, Direction.L -> Direction.U, Direction.R -> Direction.D),
    L)
  L.nextTurn = S
  S.nextTurn = R
}

object Direction extends Enumeration {
  case class Val(diff: (Int, Int), c: Char) extends super.Val
  import scala.language.implicitConversions
  implicit def valueToDirectionVal(x: Value): Val = x.asInstanceOf[Val]
  val U: Direction.Value = Val((0, -1), '^')
  val D: Direction.Value = Val((0, 1), 'v')
  val L: Direction.Value = Val((-1, 0), '<')
  val R: Direction.Value = Val((1, 0), '>')
}
