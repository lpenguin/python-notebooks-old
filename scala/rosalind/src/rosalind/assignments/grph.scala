package rosalind.assignments

import rosalind.graphs.{Graph, GraphVisWriter, Node, SimpleGraphWriter}
import rosalind.util.FastaReader._
import rosalind.util.StringUtils._

object grph {


  def main (args: Array[String]) {
    val nodes = fromFile("/home/nikita/sources/ipnb/python-notebooks/scala/rosalind/rosalind_grph.txt")

    val graph = new Graph(nodes map ( n => new Node(n.name, n.value)) )
    val k = 3


    for(List(n1, n2) <- nodes combinations 2){
      if(n1.value overlaps (n2.value, k)){
        graph addEdge (n1.name, n2.name)
      } else if(n2.value overlaps (n1.value, k)){
        graph addEdge (n2.name, n1.name)
      }
    }

    val name = "grph"
    GraphVisWriter write (graph, name, s"/home/nikita/sources/ipnb/python-notebooks/scala/rosalind/$name.gv")
    SimpleGraphWriter write (graph, name, s"/home/nikita/sources/ipnb/python-notebooks/scala/rosalind/$name.text")
  }

}