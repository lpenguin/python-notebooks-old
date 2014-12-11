package rosalind.graphs

import scala.collection.mutable


/**
 * Created by nikita on 10.12.14.
 */
case class Node(name:String, value:String){
  def ==(n:Node): Boolean ={
    name == n.name
  }

  def shortValue(k:Int) =
    if(value.length <= k+3) value
    else value.take(k)+"..."+value.takeRight(k)
}
case class Edge(n1:Node, n2:Node) {
  def ==(e:Edge):Boolean = e.n1 == n1 && e.n2 == n2
}
class Graph(val nodesList:List[Node]) {
  private val _nodes = new mutable.MutableList[Node]()
  _nodes ++= nodesList

  private val _edges = new mutable.MutableList[Edge]()

  def addEdge(n1:Node, n2:Node) = {
    val edge = new Edge(n1, n2)
    _edges.find((e) => e == edge) match {
      case Some(e) => throw new Error("Edge already exists")
      case None => _edges += edge
    }
  }

  def addEdge(n1:String, n2:String) = {
    val e1 = _nodes.find((e) => e.name == n1).get
    val e2 = _nodes.find((e) => e.name == n2).get

    val edge = new Edge(e1, e2)
    _edges.find((e) => e == edge) match {
      case Some(e) => throw new Error("Edge already exists")
      case None => _edges += edge
    }
  }

  def edges = _edges.toStream
  def nodes = _nodes.toStream
}
