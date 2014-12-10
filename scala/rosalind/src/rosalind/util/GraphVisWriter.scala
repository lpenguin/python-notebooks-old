package rosalind.util

import java.io.FileWriter

/**
 * Created by nikita on 10.12.14.
 */


object GraphVisWriter {
  def write(graph:GraphViz, name:String, toFile:String) = {
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
      fw.write(s"  \"${node.name} [label=\"${node.name}\n${node.value}\"]\";\n")
    }

    def writeEdge(edge:Edge) = {
      fw.write(s"\"${edge.n1.name}\" -> \"${edge.n2.name}\";\n")
    }
  }


}
