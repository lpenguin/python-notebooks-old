package rosalind.assignments.stepic.chapter1

import rosalind.assignments.common.PeptideSeq._
import rosalind.util.Prelude._
import scala.io.Source

/**
 * Created by nikita on 27.01.15.
 */
object spectralconvolution {
  def main(args: Array[String]) {
    def massAlphabet(spectrum: Spectrum, n:Int):Seq[Int] = {
      val spectrumWithIndex = spectrum.sorted.zipWithIndex
      val results = for((fromMass, toIndex) <- spectrumWithIndex;
            (mass, index) <- spectrumWithIndex if index < toIndex && fromMass != mass) yield fromMass - mass
      val grouped = results groupBy (x => x)
      def onlyBig(item:(Int, Int)):Boolean = {
        item._1 >= 57 && item._1 <= 200
      }
      println(grouped.toList map (x => x._1 -> x._2.size) filter onlyBig sortBy (-_._2))
      takeWithTies(grouped.toList map (x => x._1 -> x._2.size) filter onlyBig sortBy (-_._2), n)(_._2) map (_._1)
    }

    def takeWithTies[A](items:List[A], n:Int)(f: A => Int):List[A] = {
      if(items.nonEmpty){
          val (top, other) = items splitAt n
          val last = f(top.last)
          top ::: other.takeWhile( t => f(t) == last)
      }else{
        items
      }
    }

    def spectralConvolutionPeptideSearch(spectrum: Spectrum, n:Int, m:Int):List[Peptide] = {
      val alphabet = massAlphabet(spectrum, m)
      println(alphabet mkString " ")
      def expandPeptides(candidates:List[Peptide]):List[(Int, Peptide)] = {
        candidates flatMap (expand(_, alphabet)) map (peptide => linearScore(peptide, spectrum) -> peptide) sortBy (-_._1)
      }

      var leaderBoard = List[Peptide](EmptyPeptide)
      var matched = List[Peptide]()
      val parentMass = spectrum.max

      while(leaderBoard.nonEmpty){
        val closest = expandPeptides(leaderBoard) map ( p => (p._2.sum, p) ) filterNot (p => p._1 > parentMass)
        val equal = closest filter (_._1 == parentMass)
        leaderBoard = takeWithTies(closest map (_._2), n)(x => x._1) map (_._2)
        matched = (equal map (_._2._2)) ::: matched
      }
      filterMax(matched)(cyclicScore(_, spectrum))
    }

    val data = Source.fromFile("./data/dataset_104_8.txt").getLines().toList
    val List(m, n, spectrumStr) = data
    val spectrum = spectrumFromString(spectrumStr).toList

    val foundPeptides = spectralConvolutionPeptideSearch(spectrum, n.toInt, m.toInt)
    println(s"Found peptides: ${foundPeptides.size}")

    for(peptide <- foundPeptides){
      println(s"${peptide mkString "-"} ${cyclicScore(peptide, spectrum)}")
    }
    println(s"Spectrum (${spectrum.size}): ${spectrum.sorted.mkString("-")}")
    println((foundPeptides map (_.mkString("-"))).mkString("\n"))
  }
}
