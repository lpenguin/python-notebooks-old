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

    def longestSuffix(t:Tree, matchNums:Set[Int]) = {
      def stringNum(n:TreeNode) = n.label takeRight 1 toInt

      var maxLen = 0
      var suffixes = Set[String]()

      def iterNode(n:TreeNode, prevSuffix:String): Set[Int] = {
        val suffix = prevSuffix + n.label
        val commonLen = suffix.length

        val nums = n.leafs match {
          case Nil => Set(stringNum(n))
          case xs => xs flatMap (l => iterNode(l, suffix)) toSet
        }
        if(nums == matchNums){
          if(commonLen == maxLen){
            suffixes = suffixes + suffix
          }else {
            if(commonLen > maxLen){
              maxLen = commonLen
              suffixes = Set[String](suffix)
            }
          }
        }
        nums
      }
      iterNode(t.root, "")
      suffixes
    }

    def addToTree(t:Tree, suffix:String, suffixIndex:Int):Unit = {
//      println(s"addToTree $suffix $suffixIndex")
      def addToNode(node: TreeNode, matchRes:(String, String, String)): Unit ={
        val (common:String, nodeRest:String, stringRest:String) = matchRes
        if(nodeRest.nonEmpty){
          node.label = common
          node.leafs = TreeNode(nodeRest, node.leafs, node.suffixIndex) :: TreeNode(stringRest, suffixIndex) :: Nil
          node.suffixIndex = None
        }else{
          findMatch(node.leafs, stringRest) match {
            case Some(x) =>
              addToNode(x._1, x._2)
            case None =>
              node.addLeaf(TreeNode(stringRest, suffixIndex))
          }
        }
      }
      addToNode(t.root, ("", "", suffix))
      t.root.leafs = t.root.leafs filter ( !_.label.matches("^\\d+"))
    }

    def addSuffixes(t:Tree, s:String) = {
      def iter(s:Stream[Char]):List[String] = s match {
        case Empty => Nil
        case x#::xs => s.mkString :: iter(xs)
      }
      val suffixStream = iter((s).toStream)
      for((suffix, suffixIndex) <- suffixStream.reverse.zipWithIndex) {
        addToTree(t, suffix, suffixIndex)
      }
    }

    def suffixTree(strs:Seq[String]) = {
      val tree = new Tree(TreeNode(""))
      var startTime = System.currentTimeMillis()
      for((str, index) <- strs.zipWithIndex){
        println(s"Adding string #$index ${(System.currentTimeMillis() - startTime)/1000f}")
        startTime = System.currentTimeMillis()
        addSuffixes(tree, str+index.toString)
      }
      GraphVisWriter.write(tree, "lscm", s"data/lscm.gv")
      tree
    }

    def longestCommonString(strs:Seq[String]):Set[String] = {
      val tree = suffixTree(strs)
      longestSuffix(tree, strs.indices.toSet)
    }

    val data = FastaReader.fromData("rosalind_lcsm")

    val strings = data map (_.value)
    println(strings.size)
    suffixTree(strings)
//    println(longestCommonString(data map (_.value)).head)

//    addToTree(tree, "abac")
//    addToTree(tree, "abd")
//    addToTree(tree, "ace")
//    addToTree(tree, "abq")


  }
}
