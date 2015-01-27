package rosalind

import rosalind.assignments.stepic.common.PeptideSeq
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

    val p = ("114-147-115-71-97-147-113-128-99-163-128" split "-" map (_.toInt)).toList
    val p2 = ("71-97-147-113-128-99-163-128-114-147-115" split "-" map (_.toInt)).toList

    def cutTopCandidates(takeN:Int, candidates:List[(Int, Peptide)]):List[Peptide] = {
      candidates match {
        case Nil => Nil
        case _ =>
          val (top, other) = candidates splitAt takeN
          val last = top.last._1
          top ::: other.takeWhile( t => t._1 == last) map (_._2)
      }
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

    def peptideSearch(spectrum: Spectrum, takeN:Int)(trim: (Int, List[(Int, Peptide)])=> List[Peptide]):List[Peptide] = {
      def expandPeptides(candidates:List[Peptide]):List[(Int, Peptide)] = {
        println(s"Expanding list of ${candidates.size} leaderBoard")
        val expanded = candidates flatMap (peptide => expand(peptide)) map (peptide => linearScore(peptide, spectrum) -> peptide) sortBy (-_._1)
        for((pep, index) <- expanded.zipWithIndex) {
          if(pep._2 == p2){
            println("Found p2!!")
          }
          if(pep._2 == p.takeRight(pep._2.size)){
            println(s"Found p #$index with score ${pep._1} of max ${expanded.head._1}: ${pep._2 mkString "-"}")
            println(expanded.head._2.mkString("-"))
          }
        }
        expanded
//        println(s"Expanded: ${expanded.size}")
//        expanded foreach {t => println(s"${t._1} -> ${t._2 mkString "-"}")}
      }



      var leaderBoard = List[Peptide](EmptyPeptide)
      var matched = List[Peptide]()
      val searchedPeptideMass = spectrum.last

      var iterationsCount = 1
      while(leaderBoard.nonEmpty){
        println(s"iteration $iterationsCount:")
        val closest = expandPeptides(leaderBoard) map ( p => (p._2.sum, p) ) filterNot (_._1 > searchedPeptideMass)
//        closest foreach { p => println(p._2 mkString "-") }
        iterationsCount += 1
        val equal = closest filter (_._1 == searchedPeptideMass)
        leaderBoard = trim(takeN, closest map (_._2))
//        for((s, p) <- equal) println(cyclicScore(p, spectrum))
        println("Adding new found expandPeptides: "+equal.size)
        matched = (equal map (_._2._2)) ::: matched
      }
      filterMax(matched)(cyclicScore(_, spectrum))
//      matched.filter(cyclicScore(_, spectrum) == 83)
    }
    val data = Source.fromFile("./data/dataset_102_9.txt").getLines().toList
    val List(takeNStr, spectrumStr) = data
    val spectrum = spectrumFromString(spectrumStr)
    val takeN = takeNStr.toInt

    //    val peptide = peptideFromString("NQGVEWL")
    //    val takeN = 10
    //    val spectrum = cyclicSpectrum(peptide).sorted
    //    println(s"Peptide: ${peptide mkString "-"}")
    println(s"Spectrum: ${spectrum mkString "-"}")


    println(s"drop every: $takeN")
    val result = peptideSearch(spectrum, takeN)(cutTopCandidates)

    println(s"Found: ${result.size} peptides")
    result foreach { candidate =>
      println(s"Peptide ${candidate mkString "-"}: score ${cyclicScore(candidate, spectrum)} ${cyclicSpectrum(candidate).sorted mkString "-"}")
    }


    println(result map (_.mkString("-")) mkString " ")
    println(s"Max score: ${spectrum.size}")
    val q = "97-186-147-114-128-163-99-128-113".split("-").map(_.toInt).toList
    println(s"score: ${linearScore(q, spectrum)}, spectrum: ${linearSpectrum(q).sorted mkString "-"}")
//    println(cyclicSpectrum(List(57, 128, 128)).sorted mkString "-")
//    println(cyclicSpectrum(List(57, 71, 114)).sorted mkString "-")
//    println(result.head mkString "-")


  }


}
