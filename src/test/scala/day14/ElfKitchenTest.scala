package day14

import org.scalatest.flatspec.AnyFlatSpec

class ElfKitchenTest extends AnyFlatSpec {
  behavior of "shortToString"
  it should "handle simple examples" in {
    assert(ElfKitchen.shortsToString(Seq(1, 3, 5, 7)) == "1357")
  }

  behavior of "numToRecipeNodes"
  it should "handle simple examples" in {
    assert(ElfKitchen.numToRecipeNodes(1357)._1.value == 1)
  }

  behavior of "generate"
  it should "handle simple examples" in {
    val kitchen = new ElfKitchen(Seq(3, 7))
    assert(kitchen.elfLocations.head.value == 3)
    assert(kitchen.elfLocations(1).value == 7)
    kitchen.generate()
    assert(kitchen.elfLocations.head.value == 3)
    assert(kitchen.elfLocations(1).value == 7)
    assert(kitchen.lastNode.value == 0)
    kitchen.generate()
    kitchen.generate()
    assert(kitchen.elfLocations.head.value == 1)
    assert(kitchen.elfLocations.head.right == null)
    assert(kitchen.elfLocations(1).value == 1)
  }

  behavior of "sliceRecipes"
  it should "handle the examples" in {
    assert(ElfKitchen.shortsToString(new ElfKitchen(Seq(3, 7)).sliceRecipes(9, 10)) == "5158916779")
    assert(ElfKitchen.shortsToString(new ElfKitchen(Seq(3, 7)).sliceRecipes(5, 10)) == "0124515891")
    assert(ElfKitchen.shortsToString(new ElfKitchen(Seq(3, 7)).sliceRecipes(18, 10)) == "9251071085")
    assert(ElfKitchen.shortsToString(new ElfKitchen(Seq(3, 7)).sliceRecipes(2018, 10)) == "5941429882")
  }
  it should "demonstrate performance 1000" in {
    assert(ElfKitchen.shortsToString(new ElfKitchen(Seq(3, 7)).sliceRecipes(1000, 10)).length == 10)
  }
  it should "demonstrate performance 10000" in {
    assert(ElfKitchen.shortsToString(new ElfKitchen(Seq(3, 7)).sliceRecipes(10000, 10)).length == 10)
  }
  it should "demonstrate performance 20000" in {
    assert(ElfKitchen.shortsToString(new ElfKitchen(Seq(3, 7)).sliceRecipes(20000, 10)).length == 10)
  }
  it should "solve the puzzle" in {
    assert(ElfKitchen.shortsToString(new ElfKitchen(Seq(3, 7)).sliceRecipes(637061, 10)) == "3138510102")
  }

  behavior of "countUntil"
  it should "handle the examples" in {
    assert(new ElfKitchen(Seq(3, 7)).countUntil(ElfKitchen.stringToShorts("51589")) == 9)
    assert(new ElfKitchen(Seq(3, 7)).countUntil(ElfKitchen.stringToShorts("01245")) == 5)
    assert(new ElfKitchen(Seq(3, 7)).countUntil(ElfKitchen.stringToShorts("92510")) == 18)
    assert(new ElfKitchen(Seq(3, 7)).countUntil(ElfKitchen.stringToShorts("59414")) == 2018)
  }
  it should "solve the puzzle" in {
    assert(new ElfKitchen(Seq(3, 7)).countUntil(ElfKitchen.stringToShorts("637061")) == 20179081)
  }
}
