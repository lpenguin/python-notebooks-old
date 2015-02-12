package rosalind

import rosalind.assignments.stepic.common.PeptideSeq
import rosalind.assignments.stepic.common.PeptideSeq._
import rosalind.util.Prelude._

import scala.collection.SortedMap
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
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



    def peptideSearch(spectrum: Spectrum, takeN:Int)(trim: (Int, List[(Int, Peptide)])=> List[Peptide]):List[Peptide] = {
      def expandPeptides(candidates:List[Peptide]):List[(Int, Peptide)] = {
        println(s"Expanding list of ${candidates.size} leaderBoard")
        candidates flatMap (peptide => expand(peptide)) map (peptide => linearScore(peptide, spectrum) -> peptide) sortBy (-_._1)
//        println(s"Expanded: ${expanded.size}")
//        expanded foreach {t => println(s"${t._1} -> ${t._2 mkString "-"}")}
      }



      var leaderBoard = List[Peptide](EmptyPeptide)
      var matched = List[Peptide]()
      val searchedPeptideMass = spectrum.last

      var iterationsCount = 1
      while(leaderBoard.nonEmpty){
        println(s"iteration $iterationsCount:")
        val closest = expandPeptides(leaderBoard) map ( p => (p._2.sum, p) ) filterNot (p => p._1 > searchedPeptideMass)
//        closest foreach { p => println(p._2 mkString "-") }
        iterationsCount += 1
        val equal = closest filter (_._1 == searchedPeptideMass)
        leaderBoard = trim(takeN, closest map (_._2))
//        for((s, p) <- equal) println(cyclicScore(p, spectrum))
        println("Adding new found expandPeptides: "+equal.size)
        matched = (equal map (_._2._2)) ::: matched
      }
      println(s"Matched: ${matched.size}")
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

    def dummyPeptideSearch(spectrum: Spectrum, n:Int):List[Peptide] = {
      def expandPeptides(leaderboad:List[Peptide]):List[Peptide] = {
        leaderboad flatMap (peptide => expand(peptide))
      }

      def mass(peptide: Peptide) = peptide.sum

      def trim(leaderboard:ListBuffer[Peptide], n:Int):ListBuffer[Peptide] = {
        val sorted = leaderboard.sortBy(p => -linearScore(p, spectrum))
        if(sorted.size < n){
          sorted
        }else{
          var found = false
          val lastScore = linearScore(sorted(n-1), spectrum)
          var grabIndex = n - 1
          for(i <- n until sorted.size){
            if(!found){
              if(linearScore(sorted(i), spectrum) < lastScore){
                grabIndex = i - 1
                found = true
              }
            }
          }
          sorted take (grabIndex + 1)
        }

      }

      def cutTopCandidates(leaderboard:ListBuffer[Peptide], n:Int):ListBuffer[Peptide] = {
        if(leaderboard.isEmpty) {
          leaderboard
        }else {
          val sorted = leaderboard.sortBy(p => -linearScore(p, spectrum))
          val (top, other) = sorted.toList splitAt n
          val last = linearScore(top.last, spectrum)
          (top ::: other.takeWhile( t => linearScore(t, spectrum) == last)).to[ListBuffer]
        }
      }

      var leaderboard = ListBuffer[Peptide](Nil)
      val matchedPeptides = ListBuffer[Peptide](Nil)
      val parentMass = spectrum.max

      while(leaderboard.nonEmpty){
        leaderboard = expandPeptides(leaderboard.toList).to[ListBuffer]
        for(peptide <- leaderboard){
          if(mass(peptide) == parentMass){
            matchedPeptides += peptide
          }else{
            if(mass(peptide) > parentMass){
              leaderboard -= peptide
            }
          }
        }
        leaderboard = trim(leaderboard, n)
      }

      matchedPeptides.toList.filter(p => cyclicScore(p, spectrum) == 83).sortBy(p => -cyclicScore(p, spectrum))
    }

    val ress = dummyPeptideSearch(spectrum, takeN)

    println(s"Found: ${result.size} peptides")
    ress foreach { candidate =>
      println(s"Peptide ${candidate mkString "-"}: score ${cyclicScore(candidate, spectrum)} ${cyclicSpectrum(candidate).sorted mkString "-"}")
    }


    println(ress diff result)
    println(result diff ress)

    println(ress map (_.mkString("-")) mkString " ")
  }


}