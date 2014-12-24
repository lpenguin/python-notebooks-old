package rosalind.graphs

import java.io.FileWriter

/**
 * Created by nikita on 11.12.14.
 */
object SimpleGraphWriter extends GraphWriter{
  override def write(graph: Graphable, name: String, toFile: String): Unit = {
    val fw = new FileWriter(toFile)
    for(edge <- graph.edges){
      fw.write(s"${edge.n1.id} ${edge.n2.id}\n")
    }
    fw.close()
  }
}
