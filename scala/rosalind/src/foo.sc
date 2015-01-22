//import rosalind.util.Spectrum
import rosalind.assignments.stepic.common.PeptideSeq
object foo {
  val peptide = PeptideSeq.peptideFromString("NQEL")
  val spectrum = "0 99 113 114 128 227 257 299 355 356 370 371 484" split "\\s+" map (_.toInt)
  peptide foreach println
}