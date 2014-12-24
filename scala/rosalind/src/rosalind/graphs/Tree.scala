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
case class TreeNode(var label: String, private val _leafs: List[TreeNode] = Nil) extends rosalind.graphs.Node{
  def addLeaf(n:TreeNode) = listBuffer += n

  def setLeafs(ls:List[TreeNode]) = {
    listBuffer.clear()
    listBuffer ++= ls
  }

  private var listBuffer = new ListBuffer[TreeNode]
  listBuffer ++= _leafs
  def leafs = listBuffer.toList

  val id: String = SeqInt.next().toString
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
