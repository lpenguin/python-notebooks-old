package rosalind.assignments

/**
 * Created by nikita on 16.12.14.
 */
object iev {
  def main(args: Array[String]) {
    val counts = "16737 16344 19164 19866 17640 16477" split "\\s+" map (_.toInt)
    val probs =  List(1f, 1f, 1f, 0.75f, 0.5f, 0f)
    println((probs, counts).zipped map ((p, c) => c * 2 * p) sum)
  }
}
