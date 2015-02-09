package rosalind.assignments.stepic

import scala.collection.immutable.Stream.Empty
import scala.io.Source

/**
 * Created by nikita on 04.01.15.
 */
object skew {
  def main(args: Array[String]) {
    def skew(genome:Stream[Char], gc:Int = 0):List[Int] = genome match {
      case Empty => Nil
      case x#::xs =>
        val ngc = gc + (if(x == 'G') 1 else 0) - (if(x == 'C') 1 else 0)
        (ngc)::skew(xs, ngc)
    }
//    val genome = "TAAAGACTGCCGAGAGGCCAACACGAGTGCTAGAACGAGGGGCGTAAACGCGGGTCCGAT  "
      val genome = Source.fromFile("./data/e_coli.txt").mkString
//    println(0::skew(genome.toStream))

    val skewd =
      genome.foldLeft(0::Nil)((acc, x) => {
        val ngc = acc.head + (if(x == 'G') 1 else 0) - (if(x == 'C') 1 else 0)
        ngc::acc
      }).reverse
    val min = skewd.min
    println(skewd.zipWithIndex filter(_._1 == min) map (_._2) mkString " ")

  }
}
