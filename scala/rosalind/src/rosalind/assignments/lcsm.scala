package rosalind.assignments

import rosalind.graphs.{TreeNode, Tree, GraphVisWriter, Graph}
import rosalind.util.FastaReader

import scala.collection.immutable.Stream.Empty

/**
 * Created by nikita on 23.12.14.
 */
object lcsm {
  /*
  http://rosalind.info/problems/lcsm/
   */

  def prefixDiff(s1:Stream[Char], s2:Stream[Char]):(String, String, String) = {
    val marker = '-'
    val (eq, noteq) = s1 zipAll (s2, marker, marker) span (t => t._1 == t._2)
    val (d1, d2) = noteq.unzip
    (eq map (_._1) mkString,
      d1 takeWhile ( _ != marker ) mkString, d2 takeWhile ( _ != marker ) mkString)
  }


  def findMatch(leafs:List[TreeNode], str:String) = {
    leafs.foldLeft[Option[(TreeNode, (String, String, String))]](None) {(acc, leaf) =>
      acc match {
        case Some(_) => acc
        case None =>
          val (c, n, s) = prefixDiff(leaf.label.toStream, str.toStream)
          if(!c.isEmpty){
            Some(leaf, (c, n, s))
          }else{
            None
          }
      }
    }
  }

  def main(args: Array[String]) {

    def addToTree(t:Tree, s:String):Unit = {
      def addToNode(node: TreeNode, matchRes:(String, String, String)): Unit ={
        val (common:String, nodeRest:String, stringRest:String) = matchRes
        if(nodeRest.nonEmpty){
          node.label = common
          node.setLeafs(TreeNode(nodeRest, node.leafs) :: TreeNode(stringRest) :: Nil)
        }else{
          findMatch(node.leafs, stringRest) match {
            case Some(x) =>
              addToNode(x._1, x._2)
            case None =>
              node.addLeaf(TreeNode(stringRest))
          }
        }
      }
      addToNode(t.root, ("", "", s))
    }

    def addSuffixes(t:Tree, s:String) = {
      def iter(s:Stream[Char]):List[String] = s match {
        case Empty => Nil
        case x#::xs => s.mkString :: iter(xs)
      }
      iter((s+"$").toStream).reverse.foreach {
        addToTree(t, _)
      }
    }


    val data = FastaReader.fromData("rosalind_lcsm_sample")

//    val root = TreeNode("root", List(
//      TreeNode("a", List(
//        TreeNode("b"), TreeNode("c"), TreeNode("d")
//      )),
//      TreeNode("e")
//    ))
    val tree = new Tree(TreeNode("#"))
    addSuffixes(tree, "ABAB")

//    addToTree(tree, "abac")
//    addToTree(tree, "abd")
//    addToTree(tree, "ace")
//    addToTree(tree, "abq")
    for(n <- tree.nodes){
      println(n)
    }
    GraphVisWriter.write(tree, "lscm", s"data/lscm.gv")
  }
}
