package rosalind.graphs

/**
 * Created by nikita on 23.12.14.
 */

trait Node{
  def id:String
  def label:String

  def ==(n:Node): Boolean ={
    id == n.id
  }

  def shortValue(k:Int) =
    if(label.isEmpty) id
    else if(label.length <= k+3) label
    else label.take(k)+"..."+label.takeRight(k)

  override def toString = s"$id - ${shortValue(3)}"
}

case class Edge(n1:Node, n2:Node){
  assert(n1 != n2, "No circular nodes")
  def ==(e:Edge):Boolean = e.n1 == n1 && e.n2 == n2
}

trait Graphable {
  def nodes:Iterable[Node]
  def edges:Iterable[Edge]
}
