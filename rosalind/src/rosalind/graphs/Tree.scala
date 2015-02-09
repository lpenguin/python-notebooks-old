package rosalind.graphs

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created by nikita on 23.12.14.
 */
object SeqInt{
  private val n:Stream[Int] = 0 #:: n.map( n=> n+1)
  private var q = n

  def next():Int = {
    val r = q.head
    q = q.tail
    r
  }
}

object TreeNode{
  def apply(value: Stream[Char]) = new TreeNode(value, None)
  def apply(value: Stream[Char], stringIndex:Int) = new TreeNode(value, Some(stringIndex))
  def apply(value: Stream[Char], stringIndex:Int, leafs: List[TreeNode]) = new TreeNode(value, Some(stringIndex), leafs)
  def apply(value: Stream[Char], stringIndex:Option[Int], leafs: List[TreeNode]) = new TreeNode(value, stringIndex, leafs)
}

class TreeNode(var value: Stream[Char], var stringIndex:Option[Int], _leafs: List[TreeNode] = List.empty) extends rosalind.graphs.Node{
  def addLeaf(n:TreeNode) = leafBuffer += n

  def leafs_=(ls:List[TreeNode]) = {
    leafBuffer.clear()
    leafBuffer ++= ls
  }

  private var leafBuffer = new ListBuffer[TreeNode] ++ _leafs

  def leafs = leafBuffer.toList

  val id: String = SeqInt.next().toString

  override def toString = stringIndex match {
    case None => value.mkString
    case Some(i) => value.mkString +" "+i
  }

  def isLeaf = leafBuffer.isEmpty
}

class Tree(val root:TreeNode) extends Graphable{
  override def nodes: Iterable[TreeNode] = {
    def recNodes(n:TreeNode):Stream[TreeNode] = {
      val allLeafs = n.leafs.toStream.flatMap( l=> recNodes(l))
      n #:: allLeafs
    }
    recNodes(root)
  }

  override def edges: Iterable[Edge] = {
    def recEdges(n:TreeNode):Stream[Edge] = {
      n.leafs.toStream flatMap (l => Edge(n, l) #:: recEdges(l))
    }

    recEdges(root)
  }
}
