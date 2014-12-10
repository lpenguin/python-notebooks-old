package rosalind
object grph {
  import rosalind.util.{GraphVisWriter, Node, GraphViz}
  import rosalind.util.ListCombinatorics._
  import rosalind.util.StringUtils._
  import rosalind.util.FastaReader._
  val nodes = readFasta("/home/nikita/sources/ipnb/python-notebooks/scala/rosalind/rosalind_grph.txt")
  val graph = new GraphViz(nodes.map((n) => new Node(n.name, n.value)))
  val k = 3                                       //> k  : Int = 3
  //> fw  : java.io.FileWriter = java.io.FileWriter@30a4fe7c

  for(List(n1, n2) <- nodes choose 2){
    if(n1.value.overlaps(n2.value, k))
      graph.addEdge(n1.name, n2.name)
  }

  val name = "grph"
  GraphVisWriter.write(graph, name, s"home/nikita/sources/ipnb/python-notebooks/scala/rosalind/$name")

}