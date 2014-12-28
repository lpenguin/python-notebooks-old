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
  def apply(label:String) = new TreeNode(label)
  def apply(label:String, suffixIndex:Int) = new TreeNode(label, Nil, Some(suffixIndex))
  def apply(label:String, leafs: List[TreeNode]) = new TreeNode(label, leafs)
  def apply(label:String, leafs: List[TreeNode], suffixIndex:Option[Int]) = new TreeNode(label, leafs, suffixIndex)
}

class TreeNode(var label: String, _leafs: List[TreeNode] = List.empty, var suffixIndex:Option[Int] = None) extends rosalind.graphs.Node{
  def addLeaf(n:TreeNode) = leafBuffer += n

  def leafs_=(ls:List[TreeNode]) = {
    leafBuffer.clear()
    leafBuffer ++= ls
  }

  private var leafBuffer = new ListBuffer[TreeNode] ++ _leafs

  def leafs = leafBuffer.toList

  val id: String = SeqInt.next().toString

  override def toString = suffixIndex match {
    case _ => label
//    case Some(s) => s"$label ($s)"
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
