package rosalind

import rosalind.assignments.stepic.common.PeptideSeq._
import rosalind.util.Prelude._

import scala.collection.SortedMap
import scala.collection.immutable.HashMap

//import scala.collection.mutable

/**
 * Created by nikita on 22.01.15.
 */
object footest {
  def main(args: Array[String]) {
    val peptide = peptideFromString("NQEL")
    val spectrum = spectrumFromString("0 99 113 114 128 227 257 299 355 356 370 371 484")

    println(cyclicSpectrum(peptide).toList)
//    println(Spectrum.cyclicSpectrum(peptide))

    println(linearSpectrum(peptide).toList)
//    println(Spectrum.linearSpectrum(peptide).toList)
    println(score(peptide, spectrum))
//    println(Spectrum.score(peptide, spectrum))
    println(peptide)
//    println(Spectrum.expand(peptide).toList diff PeptideSeq.expand(peptide).toList )
//    println(PeptideSeq.expand(peptide).toList diff Spectrum.expand(peptide).toList )
    println(List(1, 2, 3) == List(1, 2, 3))



    var ms = SortedMap(0 -> 1)

    val m = (expand(EmptyPeptide) map (x => score(x, spectrum) -> x)).toMap

    println(m)

    expand(EmptyPeptide).size.println
  }


}
