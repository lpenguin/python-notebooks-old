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
    val peptide = peptideFromString("NQEL")
//    val spectrum = spectrumFromString("0 99 113 114 128 227 257 299 355 356 370 371 484")
//    val spectrum = spectrumFromString("0 71 113 129 147 200 218 260 313 331 347 389 460")
    val data = Source.fromFile("./data/rosalind_2f.txt").getLines().toList
    val List(takeNStr, spectrumStr) = data
    val spectrum = spectrumFromString(spectrumStr)
    val takeN = takeNStr.toInt
//    println(cyclicSpectrum(peptide).toList)
////    println(Spectrum.cyclicSpectrum(peptide))
//
//    println(linearSpectrum(peptide).toList)
////    println(Spectrum.linearSpectrum(peptide).toList)
//    println(score(peptide, spectrum))
////    println(Spectrum.score(peptide, spectrum))
//    println(peptide)
////    println(Spectrum.expand(peptide).toList diff PeptideSeq.expand(peptide).toList )
////    println(PeptideSeq.expand(peptide).toList diff Spectrum.expand(peptide).toList )
//    println(List(1, 2, 3) == List(1, 2, 3))
//
//
//    var ms = SortedMap[Int, List[Peptide]]()
//
//    val scores = expand(EmptyPeptide) map (x => score(x, spectrum) -> x) sortBy (_._1)
//    scores foreach println
//    val groupedScores = scores groupBy {case (score, _) => score}
//
//    val testSet = for(i <- 1 to 100) yield Random.nextInt(100) -> i
//
//
//
//    scores groupOrdered (x => x._1)



    def peptideSearch(spectrum: Spectrum, takeN:Int):List[Peptide] = {
      def expandAndTakeClosest(candidates:List[Peptide]):List[Peptide] = {
        def cut(candidates:List[(Int, Peptide)]):List[(Int, Peptide)] = {
          val (top, other) = candidates splitAt takeN
          val last = top.last._1
//          top foreach println
          top ::: other.takeWhile( t => t._1 == last)
        }
        val expandedSet = candidates flatMap (peptide => expand(peptide)) map (peptide => score(peptide, spectrum) -> peptide) sortBy (-_._1)
        val t = expandedSet groupOrdered (_._1)
//        println("Expanded: ")
//        t foreach println
        t take takeN flatMap (_._2 map (_._2))
//        cut(expandedSet) map (_._2)
      }



      var candidates = List[Peptide](EmptyPeptide)
      var matched = List[Peptide]()
      val searchedPeptideMass = spectrum.last

      var iterationsCount = 1
      while(candidates.nonEmpty){
        val closest = expandAndTakeClosest(candidates) map ( p => (p.sum, p) ) filterNot (_._1 > searchedPeptideMass)
        println(s"iteration $iterationsCount: found closest candidates: ${closest.size}")
        iterationsCount += 1
        val (less, equal) = closest partition (_._1 < searchedPeptideMass)
        candidates = less map (_._2)
        matched = (equal map (_._2)) ::: matched
      }
      filterMax(matched)(score(_, spectrum))
    }

    //113-147-71-129
    println(s"drop every: $takeN")
    val result = peptideSearch(spectrum, takeN)
    println(result.size)
    result foreach { candidate =>
      println(s"$candidate: ${score(candidate, spectrum)}")
    }

    println(result map (_.mkString("-")) mkString " ")

//    println(result.head mkString "-")


  }


}
