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
//    tree add data.map(_.value)

//    tree.longestCommonString(data.map(_.value)).foreach( x => println(x))
    1 to 12 foreach { i =>
      tree add "abc"+"$"
    }
//    tree.stringIndicies.toList.sorted foreach println

//    val sd = prefixDiff2("abcde".toStream, "abcxy".toStream)
//    println(s"common: ${sd.common.mkString} s1: ${sd.diff1.mkString} s2: ${sd.diff2.mkString}")
//    tree addSuffix "abc1"
//    tree addSuffix "abcde2"
//    tree addSuffix "abe3"





//    println("Build tree")
    TreePrinter.print(tree)
  }
}
