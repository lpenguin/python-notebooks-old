package rosalind.assignments

import rosalind.graphs.{SuffixTree, GraphVisWriter, TreeNode, Tree}
import rosalind.util.{TreePrinter, FastaReader}
import rosalind.util.Profiler._
import rosalind.graphs.tparty.SimpleSuffixTree
import sun.misc.GC

import scala.collection.immutable.Stream.Empty
import scala.collection.mutable.ListBuffer

/**
 * Created by nikita on 28.12.14.
 * http://rosalind.info/problems/lcsm/
 */
object lcsm2 {
  def main(args: Array[String]) {
    val data = FastaReader.fromData("rosalind_lcsm")

    val tree = new Tree(TreeNode("/".toStream)) with SuffixTree
    tree.longestCommonString(data.map(_.value)).foreach( x => println(x))
    traceMem()

//    TreePrinter.print(tree)
  }
}
