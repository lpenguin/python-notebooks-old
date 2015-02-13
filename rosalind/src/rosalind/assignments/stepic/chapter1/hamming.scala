package rosalind.assignments.stepic.chapter1

import rosalind.util.StringUtils._

import scala.io.Source
/**
 * Created by nikita on 04.01.15.
 */
object hamming {
  def main(args: Array[String]) {
    val List(s1, s2) = Source.fromFile("./data/stepic_hamming_1.txt").getLines().toList

    println(s1 hamming s2)
  }
}
