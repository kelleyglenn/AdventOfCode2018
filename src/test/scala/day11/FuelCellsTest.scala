package day11

import org.scalatest.flatspec.AnyFlatSpec

class FuelCellsTest extends AnyFlatSpec {
  behavior of "gridOfPowerLevels"
  it should "match the example" in {
    assert(FuelCells.gridOfPowerLevels(8)(5 - 1)(3 - 1) == 4)
    assert(FuelCells.gridOfPowerLevels(57)(79 - 1)(122 - 1) == -5)
    assert(FuelCells.gridOfPowerLevels(39)(196 - 1)(217 - 1) == 0)
    assert(FuelCells.gridOfPowerLevels(71)(153 - 1)(101 - 1) == 4)
  }

  behavior of "mostPowerfulSquare"
  it should "handle simple cases" in {
    assert(FuelCells.mostPowerfulSquare(Seq(Seq(1, 2), Seq(3, 4)), 1) == ((2, 2), 4))
    assert(FuelCells.mostPowerfulSquare(Seq(Seq(1, 2), Seq(3, 4)), 2) == ((1, 1), 10))
    assert(FuelCells.mostPowerfulSquare(Seq(Seq(1, 2, 99), Seq(3, 4, 5), Seq(6, 7, 8)), 2) == ((2, 1), 110))
  }
  it should "handle the examples" in {
    assert(FuelCells.mostPowerfulSquare(FuelCells.gridOfPowerLevels(18)) == ((33, 45), 29))
    assert(FuelCells.mostPowerfulSquare(FuelCells.gridOfPowerLevels(42)) == ((21, 61), 30))
  }
  it should "demonstrate performance 10" in {
    assert(FuelCells.mostPowerfulSquare(FuelCells.gridOfPowerLevels(18), 10) != null)
  }
  it should "demonstrate performance 100" in {
    assert(FuelCells.mostPowerfulSquare(FuelCells.gridOfPowerLevels(18), 100) != null)
  }
  it should "demonstrate performance 150" in {
    assert(FuelCells.mostPowerfulSquare(FuelCells.gridOfPowerLevels(18), 150) != null)
  }
  it should "demonstrate performance 300" in {
    assert(FuelCells.mostPowerfulSquare(FuelCells.gridOfPowerLevels(18), 300) != null)
  }
  it should "solve the puzzle" in {
    assert(FuelCells.mostPowerfulSquare(FuelCells.gridOfPowerLevels(3999))._1 == (21, 77))
  }

  behavior of "mostPowerfulSquareSize"
  it should "handle the examples" in {
    assert(FuelCells.mostPowerfulSquareSize(FuelCells.gridOfPowerLevels(18)) == (90, 269, 16))
    assert(FuelCells.mostPowerfulSquareSize(FuelCells.gridOfPowerLevels(42)) == (232, 251, 12))
  }
  it should "solve the puzzle" in {
    assert(FuelCells.mostPowerfulSquareSize(FuelCells.gridOfPowerLevels(3999)) == (224, 222, 27))
  }
}
