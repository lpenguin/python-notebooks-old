package rosalind.assignments

import rosalind.graphs.{GraphVisWriter, TreeNode, Tree}
import rosalind.util.{TreePrinter, FastaReader}
import rosalind.util.Profiler._
import rosalind.graphs.tparty.SimpleSuffixTree

import scala.collection.immutable.Stream.Empty

/**
 * Created by nikita on 28.12.14.
 * http://rosalind.info/problems/lcsm/
 */
object lcsm2 {
  def main(args: Array[String]) {
    def suffixesList(str: String) = {
      def iter(s: List[Char]): List[List[Char]] = s match {
        case Nil => Nil
        case x :: xs => s :: iter(xs)
      }
      iter(str.toList)
    }

    def suffixes(str: String) = {
      def iter(s: Stream[Char]): Stream[Stream[Char]] = s match {
        case Empty => Empty
        case x #:: xs => s #:: iter(xs)
      }
      iter(str.toStream)
    }

    def addToTree(tree:Tree, suffix:Stream[Char]) = {
      def findNode(n:TreeNode, suffix:Stream[Char]):(TreeNode, Stream[Char]) = {
        if(suffix.isEmpty){
          (n, suffix)
        }else{
          val head = suffix.head
          n.leafs find (l => l.label.head == head) match {
            case None => (n, suffix)
            case Some(x) => findNode(x, suffix.tail)
          }
        }
      }

      def addToNode(parent:TreeNode, suffix:Stream[Char]):Unit = suffix match {
        case Empty =>
        case x#::xs =>
          val node = TreeNode(x.toString)
          parent.addLeaf(node)
          addToNode(node, xs)
      }

      val (node, newSuffix) = findNode(tree.root, suffix)
      addToNode(node, newSuffix)
    }

    val data = FastaReader.fromData("rosalind_lcsm")

    val tree = new Tree(TreeNode("ROOT"))
//    val tree = new Tree(TreeNode("ROOT", List(
//      TreeNode("a", List(
//        TreeNode("b"), TreeNode("c"))))))

    addToTree(tree, "abc0".toStream)
    addToTree(tree, "abcda1".toStream)
    TreePrinter.print(tree)
    GraphVisWriter.write(tree, "ff", "ff.gv")
//    suffixes(data.head.value) foreach (x => println(x.mkString))
  }
}
