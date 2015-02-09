package rosalind.assignments.stepic

import scala.io.Source
import rosalind.util.StringUtils._
/**
 * Created by nikita on 04.01.15.
 */
object hamming {
  def main(args: Array[String]) {
    val List(s1, s2) = Source.fromFile("./data/stepic_hamming_1.txt").getLines().toList

    println(s1 hamming s2)
  }
}
