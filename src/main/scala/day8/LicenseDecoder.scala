package day8

object LicenseDecoder {
  case class Node(children: Seq[Node], metadata: Seq[Int]) {
    def metadataSum: Int = children.map(_.metadataSum).sum + metadata.sum

    def value: Int = {
      if (children.isEmpty) metadata.sum
      else
        metadata.filter((m: Int) => children.indices.toSet.contains(m - 1)).map((m: Int) => children(m - 1).value).sum
    }
  }

  object Node {

    def fromInts(ints: Seq[Int]): (Node, Seq[Int]) = {
      var remainder: Seq[Int] = ints.drop(2)
      var children: Seq[Node] = Seq.empty
      for (_ <- 1 to ints.head) {
        val (newNode, newRemainder) = fromInts(remainder)
        remainder = newRemainder
        children = children :+ newNode
      }
      (Node(children, remainder.take(ints.tail.head)), remainder.drop(ints.tail.head))
    }

    def fromString(s: String): Node = {
      fromInts(s.split(' ').map(_.toInt))._1
    }
  }
}
