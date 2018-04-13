package kh.scalausergroup.algorithms

import org.scalameter.api._

object SeqTreeBenchmark extends Bench.OfflineReport {

  def sizeOfTree(width: Int, depth: Int): Int = math.max(1, ((1 - math.pow(width, depth)) / (1 - width)).toInt)

  def createTestTree(prefix: String, pos: Int, width: Int, depth: Int): SeqTree[String] = {
    val value = prefix + pos
    val children = if (depth <= 1) Seq.empty else
      (0 until width).map(i => createTestTree(value, i, width, depth - 1))
    SeqTree[String](value, children)
  }

  val treeGen: Gen[(SeqTree[String], Int)] = for {
    width <- Gen.range("width")(1, 8, 1)
    depth <- Gen.range("depth")(1, 8, 1)
  } yield (createTestTree("bench", 0, width, depth), sizeOfTree(width, depth))

  performance of "applyTailRec" in {
    using(treeGen) in { case (tree, size) =>
      tree(size - 1)
    }
  }
}
