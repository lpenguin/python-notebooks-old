package rosalind.graphs

import scala.collection.immutable.Stream.Empty
import rosalind.util.Profiler._

import scala.collection.mutable.ListBuffer

/**
 * Created by nikita on 29.12.14.
 */

object SuffixTree {
  def suffixes(str: Stream[Char]) = {
    def iter(s: Stream[Char]): Stream[Stream[Char]] = s match {
      case Empty => Empty
      case x #:: xs => s #:: iter(xs)
    }
    iter(str)
  }

}
trait SuffixTree extends Tree{

  def add(strs:Seq[String]):Unit = {
    strs.zipWithIndex foreach { case (s, i) =>
      add((s+"$").toStream)
    }
  }

  def add(str:String):Unit = add(str.toStream)

  def add(str:Stream[Char]):Unit = {
    SuffixTree.suffixes(str) foreach addSuffix
    root.leafs = root.leafs filter ( !_.value.mkString.matches("^\\d+$"))
  }

  def longestCommonString(strs:Seq[String]):Set[String] = {
    add(strs)
    println(strs.indices.toSet.min)
    longestSuffix(strs.indices.toSet)
  }

  private def longestSuffix(matchNums:Set[Int]) = {
    def stringNum(n:TreeNode) = {
      n.value.takeRight(1).mkString.toInt
    }

    var maxLen = 0
    var suffixes = Set[String]()

    def iterNode(n:TreeNode, prevSuffix:String): Set[Int] = {
      val suffix = prevSuffix + n.value.mkString
      val commonLen = suffix.length

      val nums = n.leafs match {
        case Nil => Set(stringNum(n))
        case xs => xs flatMap (l => iterNode(l, suffix)) toSet
      }
      if(nums == matchNums){
        if(commonLen == maxLen){
          suffixes = suffixes + suffix.drop(1)
        }else {
          if(commonLen > maxLen){
            maxLen = commonLen
            suffixes = Set[String](suffix.drop(1))
          }
        }
      }
      nums
    }
    iterNode(root, "")
    suffixes
  }

  def stringIndicies = {
    def stringNum(n:TreeNode) = {
      n.value.takeRight(1).mkString.toInt
    }

    def iterNode(n:TreeNode, prevSuffix:String): Set[Int] = {
      val suffix = prevSuffix + n.value.mkString
      val commonLen = suffix.length

      val nums = n.leafs match {
        case Nil => Set(stringNum(n))
        case xs => xs flatMap (l => iterNode(l, suffix)) toSet
      }
      nums
    }
    iterNode(root, "")
  }


  def addSuffix(suffix:String):Unit = addSuffix(suffix.toStream)

  def addSuffix(suffix:Stream[Char]):Unit = {
    class StringDiff(val common:Stream[Char], val diff1:Stream[Char], val diff2:Stream[Char])

    def prefixDiff(s1:Stream[Char], s2:Stream[Char]):StringDiff = {
      val common = new ListBuffer[Char]

      def iter(s1:Stream[Char], s2:Stream[Char]):(Stream[Char], Stream[Char]) = {
        if(s1.isEmpty || s2.isEmpty) {
          (s1, s2)
        }else{
          if(s1.head == s2.head){
            common += s1.head
            iter(s1.tail, s2.tail)
          }else{
            (s1, s2)
          }
        }
      }
      val (diff1, diff2) = iter(s1, s2)
      new StringDiff(common.toStream, diff1, diff2)
    }

    def findMatch(leafs:List[TreeNode], str:Stream[Char]) =
      leafs.foldLeft[Option[(TreeNode, StringDiff)]](None) {(acc, leaf) =>
        acc match {
          case Some(_) => acc
          case None =>
            val stringDiff = prefixDiff(leaf.value, str)
            if(stringDiff.common.nonEmpty){
              Some(leaf, stringDiff)
            }else{
              None
            }
        }
      }

    def findNode(parent:TreeNode, suffix:Stream[Char]):(TreeNode, StringDiff) = {
      if(suffix.isEmpty){
        (parent, new StringDiff(parent.value, Empty, Empty))
      }else{
        findMatch(parent.leafs, suffix) match {
          case None => (parent, new StringDiff(parent.value, Empty, suffix))
          case Some((child, sd)) =>
            if (sd.diff1.isEmpty) {
              findNode(child, sd.diff2)
            } else {
              (child, sd)
            }
        }
      }
    }

    def addToNode(parent:TreeNode, stringDiff:StringDiff):Unit = {
      parent.value = stringDiff.common
      if(stringDiff.diff1.isEmpty){
        parent.addLeaf(TreeNode(stringDiff.diff2))
      }else{
        val leafs = parent.leafs

        parent.leafs = if(stringDiff.diff2.isEmpty) List(TreeNode(stringDiff.diff1, leafs)) else List(TreeNode(stringDiff.diff1, leafs), TreeNode(stringDiff.diff2))
      }
    }

    val(node, sdiff) = findNode(root, suffix)

    addToNode(node, sdiff)
  }
}
