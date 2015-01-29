import rosalind.assignments.stepic.common.Motif._
import rosalind.util.Prelude._

import scala.io.Source

/**
 * Created by nikita on 29.01.15.
 */
object ste3_4_9 {



  def medianString(k:Int, dnas:Seq[String]):String = {
    def distInDna(pattern: CharList, dna: String) = (dna slidingFast k map (pattern hamming _)).min

    def distInDnas(pattern: CharList, strings: Seq[String]) = (strings map (dna => distInDna(pattern, dna))).sum

    (enumeratePatterns(k) minBy (pattern => distInDnas(pattern, dnas))).mkString
  }

  def main(args: Array[String]) {
    val data = Source.fromFile("./data/t.txt").getLines().toList
    val k = data.head.toInt
    val dnas = data.tail


    println(medianString(k, dnas))
  }
}
