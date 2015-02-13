package rosalind.assignments.stepic.chapter1

import rosalind.util.Spectrum._

/**
 * Created by nikita on 11.01.15.
 */
object spectrum_score {
  def main(args: Array[String]) {
    val peptide = "NQEL"
    val spectrum = "0 99 113 114 128 227 257 299 355 356 370 371 484"
//    val List(peptide, spectrum) = Source.fromFile("./data/dataset_102_3.txt").getLines().toList

//    println(peptide)
//    println(spectrum.toList)
//    println(cyclicSpectrum(peptide))
    println(score(massesList(peptide), spectrum split "\\s+" map (_.toInt)))
    println(linearScore(massesList(peptide), spectrum split "\\s+" map (_.toInt)))

  }
}
