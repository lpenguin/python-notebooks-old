package rosalind

import scala.io.Source
import rosalind.assignments.common.PeptideSeq._
/**
 * Created by nikita on 27.01.15.
 */
object st2cstrim {
  def main(args: Array[String]) {
    val data = Source.fromFile("./data/dataset_4913_3.txt").getLines().toList
    val List(peptidesStr, spectrumStr, nStr) = data
    val peptides = (peptidesStr split "\\s+").toList
    val spectrum = spectrumFromString(spectrumStr)
    val n = nStr.toInt

    def linearScore(peptide:Peptide, spectrum: Spectrum):Int = {
      val peptideSpectrum = linearSpectrum(peptide)
      peptideSpectrum.length - (peptideSpectrum diff spectrum).length + 1
    }

    def trimTopCandidates(takeN:Int, candidates:List[(Int, String)]):List[String] = {
      candidates match {
        case Nil => Nil
        case _ =>
          val (top, other) = candidates splitAt takeN
          val last = top.last._1
          top ::: other.takeWhile( t => t._1 == last) map (_._2)
      }
    }

    val res = trimTopCandidates(n, (peptides map (p => linearScore(peptideFromString(p), spectrum)) zip peptides).sortBy(-_._1))
    println(res mkString " ")

  }
}
