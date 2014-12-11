package rosalind.graphs

/**
 * Created by nikita on 11.12.14.
 */
trait GraphWriter {
  def write(graph:Graph, name:String, toFile:String)
}
