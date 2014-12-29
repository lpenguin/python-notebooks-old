package rosalind.assignments

import rosalind.graphs.{GraphVisWriter, TreeNode, Tree}
import rosalind.util.{FastaReader}
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
      def findNode(n:TreeNode, suffix:Stream[Char]):Option[(TreeNode, Stream[Char])] = {
        if(suffix.isEmpty){
          Some((n, suffix))
        }else{
          val head = suffix.head
          n.leafs find (l => l.label.head == head) match {
            case None => Some((n, suffix))
            case Some(x) => findNode(x, suffix.tail)
          }
        }

      }
      findNode(tree.root, suffix) match {
        case None => println("None found")
        case Some(x) =>
          println(x._1.label, x._2.mkString)
      }

    }

    val data = FastaReader.fromData("rosalind_lcsm")

    val tree = new Tree(TreeNode("ROOT", List(
      TreeNode("a", List(
        TreeNode("b", List(TreeNode("c",
          List(TreeNode("d")))))))
    )))

    addToTree(tree, "abcda".toStream)
    GraphVisWriter.write(tree, "ff", "ff.gv")
//    suffixes(data.head.value) foreach (x => println(x.mkString))
  }
}
