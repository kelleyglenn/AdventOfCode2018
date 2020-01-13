package day14

class ElfKitchen(val elfRecipes: Seq[Short]) {
  var elfLocations: Seq[RecipeNode] = elfRecipes.map(new RecipeNode(_))
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

  def matchesSoFar(start: RecipeNode, shorts: Seq[Short]): (Boolean, Boolean) = {
    if (shorts.isEmpty) (true, true)
    else if (start == null) (true, false)
    else if (start.value == shorts.head) matchesSoFar(start.right, shorts.tail)
    else (false, false)
  }

  def getCandidateFrom(start: RecipeNode, shorts: Seq[Short]): (RecipeNode, Int, Boolean) = {
    var curNode: RecipeNode = start
    var skippedCt = 0
    var matches: (Boolean, Boolean) = matchesSoFar(curNode, shorts)
    while ((!matches._1) && curNode.right != null) {
      curNode = curNode.right
      skippedCt += 1
      matches = matchesSoFar(curNode, shorts)
    }
    (curNode, skippedCt, matches._2)
  }

  def countUntil(shorts: Seq[Short]): Int = {
    var (curCandidate: RecipeNode, skippedCount: Int, found: Boolean) = getCandidateFrom(firstNode, shorts)
    while (!found) {
      generate()
      val results: (RecipeNode, Int, Boolean) = getCandidateFrom(curCandidate, shorts)
      curCandidate = results._1
      skippedCount += results._2
      found = results._3
    }
    skippedCount
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

  def stringToShorts(s: String): Seq[Short] = {
    s.map((c: Char) => (c - '0').toShort)
  }

  def numToRecipeNodes(sum: Int): (RecipeNode, RecipeNode, Int) = {
    recipeNodesFromSeq(stringToShorts(sum.toString))
  }

  def recipeNodesFromSeq(seq: Seq[Short]): (RecipeNode, RecipeNode, Int) = {
    val nodes: Seq[RecipeNode] = seq.map(new RecipeNode(_))
    (0 until nodes.size - 1).foreach { i: Int =>
      assignNodes(nodes(i), nodes(i + 1))
    }
    (nodes.head, nodes.last, nodes.size)
  }
}

class RecipeNode(val value: Short, var left: RecipeNode = null, var right: RecipeNode = null)
