import rosalind.util.StringUtils
import rosalind.util.Spectrum._
object foo {
  val peptide = "NQEL"
  val spectrum = "0 99 113 114 128 227 257 299 355 356 370 371 484" split "\\s+" map (_.toInt)
//  println(cyclicSpectrum(peptide).sorted)
//  println(score(massesList(peptide), spectrum))
}