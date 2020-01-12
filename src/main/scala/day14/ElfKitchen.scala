package day14

class ElfKitchen(val elfRecipes: Seq[Short]) {
  var elfLocations: Seq[RecipeNode] = elfRecipes.map(RecipeNode(_))
  (0 until elfLocations.size - 1).foreach { i: Int =>
    ElfKitchen.assignNodes(elfLocations(i), elfLocations(i + 1))
  }
  val firstNode: RecipeNode = elfLocations.head
  var lastNode: RecipeNode = elfLocations.last

  def moveRight(from: RecipeNode, distance: Short): RecipeNode = {
    var targetNode: RecipeNode = from
    (1 to distance).foreach { _: Int =>
      targetNode = if (targetNode.right != null) targetNode.right else firstNode
    }
    targetNode
  }

  def sliceRecipes(skip: Int, keep: Int): Seq[Short] = {
    var nodeCt: Int = elfLocations.size
    while (nodeCt < skip + keep) nodeCt += generate()
    (1 to nodeCt - (skip + keep)).foreach((_: Int) => lastNode = lastNode.left)
    var shorts: Seq[Short] = Seq.empty
    (1 to keep).foreach { _: Int =>
      shorts = lastNode.value +: shorts
      lastNode = lastNode.left
    }
    shorts
  }

  def generate(): Int = {
    val (firstNew, lastNew, size) = ElfKitchen.numToRecipeNodes(elfLocations.map((n: RecipeNode) => n.value.toInt).sum)
    ElfKitchen.assignNodes(lastNode, firstNew)
    lastNode = lastNew
    elfLocations = elfLocations.map((n: RecipeNode) => moveRight(n, (n.value + 1).toShort))
    size
  }
}

object ElfKitchen {

  def assignNodes(left: RecipeNode, right: RecipeNode): Unit = {
    left.right = right
    right.left = left
  }

  def shortsToString(shorts: Seq[Short]): String = {
    shorts.map((s: Short) => ('0'.toShort + s).toChar).mkString
  }

  def numToRecipeNodes(sum: Int): (RecipeNode, RecipeNode, Int) = {
    recipeNodesFromSeq(sum.toString.map((c: Char) => (c - '0').toShort))
  }

  def recipeNodesFromSeq(seq: Seq[Short]): (RecipeNode, RecipeNode, Int) = {
    val nodes: Seq[RecipeNode] = seq.map(RecipeNode(_))
    (0 until nodes.size - 1).foreach { i: Int =>
      assignNodes(nodes(i), nodes(i + 1))
    }
    (nodes.head, nodes.last, nodes.size)
  }
}

case class RecipeNode(value: Short, var left: RecipeNode = null, var right: RecipeNode = null) {
  override def toString: String = value.toString
}
