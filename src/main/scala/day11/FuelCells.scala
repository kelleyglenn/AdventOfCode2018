package day11

object FuelCells {

  def powerLevel(cell: (Int, Int), serialNumber: Int): Int = {
    ((((((cell._1 + 10) * cell._2) + serialNumber) * (cell._1 + 10)) / 100) % 10) - 5
  }

  def gridOfPowerLevels(serialNumber: Int): Seq[Seq[Int]] = {
    (1 to 300).map((y: Int) => (1 to 300).map((x: Int) => powerLevel((x, y), serialNumber)))
  }

  def mostPowerfulSquare(grid: Seq[Seq[Int]], squareSize: Int = 3): ((Int, Int), Int) = {
    val max: ((Int, Int), Int) = (0 to grid.size - squareSize)
      .flatMap { y: Int =>
        val colSums: Seq[Int] = grid.head.indices.map { x: Int =>
          (y until y + squareSize).map((y1: Int) => grid(y1)(x)).sum
        }
        (0 to grid.head.size - squareSize).map { x1: Int =>
          ((x1, y), colSums.slice(x1, squareSize + x1).sum)
        }
      }
      .maxBy(_._2)
    ((max._1._1 + 1, max._1._2 + 1), max._2)
  }

  def mostPowerfulSquareSize(grid: Seq[Seq[Int]]): (Int, Int, Int) = {
    val max: (((Int, Int), Int), Int) =
      (1 to 300).map((size: Int) => (mostPowerfulSquare(grid, size), size)).maxBy(_._1._2)
    (max._1._1._1, max._1._1._2, max._2)
  }
}
