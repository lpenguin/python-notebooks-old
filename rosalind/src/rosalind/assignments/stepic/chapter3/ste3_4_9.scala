package rosalind.assignments.stepic.chapter3

import rosalind.assignments.common.Motif._
import rosalind.util.Extractor.{Extract, Int}
import rosalind.util.Prelude
import rosalind.util.Prelude._

/**
 * Created by nikita on 29.01.15.
 */
object ste3_4_9 {
  object MedianString extends Task{
    override def run(data: List[String]): Seq[String] = {
      data match {
        case Extract(Int(k))::dnas =>
          medianString(k, dnas)::Nil
      }
    }

    def medianString(k:Int, dnas:Seq[String]):String = {
      def distInDna(pattern: CharList, dna: String): Int =
        (dna slidingFast k map (pattern hamming _)).min

      def distInDnas(pattern: CharList): Int =
        (dnas map (dna => distInDna(pattern, dna))).sum

      val patternDists = enumeratePatterns(k) map (pattern => pattern -> distInDnas(pattern))
      patternDists minBy { case (_, dist) => dist} match {
        case (pattern, dist) => s"${pattern.mkString} $dist"
      }
    }
  }

  def main(args: Array[String]) {
    println(MedianString.run(Prelude.fromData("dataset_158_9")))
  }
}
