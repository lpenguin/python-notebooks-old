package rosalind.graphs

/**
 * Created by nikita on 11.12.14.
 */
trait GraphWriter {
  def write(graph:Graphable, name:String, toFile:String)
}
