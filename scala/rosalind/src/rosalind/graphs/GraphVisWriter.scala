package rosalind.graphs

import java.io.FileWriter

/**
 * Created by nikita on 10.12.14.
 */


object GraphVisWriter extends GraphWriter{
  def write(graph:Graph, name:String, toFile:String) = {
    val fw = new FileWriter(toFile)

    writeHeader()
    for(node <- graph.nodes){
      writeNode(node)
    }
    for(edge <- graph.edges){
      writeEdge(edge)
    }
    writeFooter()
    fw.close()

    def writeHeader() = {
      fw.write(s"digraph $name {\n")
    }

    def writeFooter() = {
      fw.write(s"}\n")
    }

    def writeNode(node:Node) = {
      fw.write("  \""+node.name+"\" [label=\""+node.name+" - "+node.shortValue(3)+"\"];\n")
    }

    def writeEdge(edge:Edge) = {
//      fw.write(s"${edge.n1.name} -> ${edge.n2.name};")
      fw.write(s"""  "${edge.n1.name}" -> "${edge.n2.name}";\n""")
    }
  }


}
