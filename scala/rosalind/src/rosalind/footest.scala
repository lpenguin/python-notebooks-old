package rosalind

import rosalind.assignments.stepic.common.PeptideSeq._
import rosalind.util.Prelude._

import scala.collection.SortedMap
import scala.collection.immutable.HashMap
import scala.io.Source
import scala.util.Random

//import scala.collection.mutable

/**
 * Created by nikita on 22.01.15.
 */
object footest {
  def main(args: Array[String]) {
    val peptide = peptideFromString("NQGAE")
//    val spectrum = spectrumFromString("0 71 113 129 147 200 218 260 313 331 347 389 460")
//    val data = Source.fromFile("./data/rosalind_2f.txt").getLines().toList
    val data = Source.fromFile("./data/dataset_102_9.txt").getLines().toList
    val List(takeNStr, spectrumStr) = data
    val spectrum = spectrumFromString(spectrumStr)
    val takeN = takeNStr.toInt



    def cutTopCandidates(takeN:Int, candidates:List[(Int, Peptide)]):List[Peptide] = {
      val (top, other) = candidates splitAt takeN
      val last = top.last._1
      top ::: other.takeWhile( t => t._1 == last) map (_._2)
    }

    def cutGroupedCandidates(takeN:Int, candidates:List[(Int, Peptide)]):List[Peptide] = {
      val t = candidates groupOrdered (_._1)
      t take takeN flatMap (_._2 map (_._2))
    }

    def linearScore(peptide:Peptide, spectrum: Spectrum):Int = {
      val peptideSpectrum = linearSpectrum(peptide)
      peptideSpectrum.length - (peptideSpectrum diff spectrum).length
    }

    def cyclicScore(peptide:Peptide, spectrum: Spectrum):Int = {
      val peptideSpectrum = cyclicSpectrum(peptide)
      peptideSpectrum.length - (peptideSpectrum diff spectrum).length
    }

    def peptideSearch(spectrum: Spectrum, takeN:Int)(cutFunc: (Int, List[(Int, Peptide)])=> List[Peptide]):List[Peptide] = {
      def expandAndTakeClosest(candidates:List[Peptide]):List[Peptide] = {
        val expanded = candidates flatMap (peptide => expand(peptide)) map (peptide => linearScore(peptide, spectrum) -> peptide) sortBy (-_._1)
//        println(s"Expanded: ${expanded.size}")
//        expanded foreach {t => println(s"${t._1} -> ${t._2 mkString "-"}")}
        cutFunc(takeN, expanded)
      }



      var candidates = List[Peptide](EmptyPeptide)
      var matched = List[Peptide]()
      val searchedPeptideMass = spectrum.last

      var iterationsCount = 1
      while(candidates.nonEmpty){
        val closest = expandAndTakeClosest(candidates) map ( p => (p.sum, p) ) filterNot (_._1 > searchedPeptideMass)
//        println(s"iteration $iterationsCount: found closest candidates: ${closest.size}")
//        closest foreach { p => println(p._2 mkString "-") }
        iterationsCount += 1
        val (less, equal) = closest partition (_._1 < searchedPeptideMass)
        candidates = less map (_._2)
        matched = (equal map (_._2)) ::: matched
      }
//      filterMax(matched)(cyclicScore(_, spectrum))
      matched.filter(cyclicScore(_, spectrum) == 83)
    }

//    val takeN = 8
//    val spectrum = cyclicSpectrum(peptide).sorted

    println(s"Peptide: ${peptide mkString "-"}")
    println(s"Spectrum: ${spectrum mkString "-"}")

    //113-147-71-129
    println(s"drop every: $takeN")
    val result = peptideSearch(spectrum, takeN)(cutTopCandidates)

    println(s"Found: ${result.size} peptides")
    result foreach { candidate =>
      println(s"Peptide ${candidate mkString "-"}: score ${cyclicScore(candidate, spectrum)}")
    }


    println(result map (_.mkString("-")) mkString " ")
    println(s"Max score: ${spectrum.size}")
//    println(cyclicSpectrum(List(57, 128, 128)).sorted mkString "-")
//    println(cyclicSpectrum(List(57, 71, 114)).sorted mkString "-")
//    println(result.head mkString "-")


  }


}
