package rosalind.assignments.stepic.common

import scala.collection.{mutable, Seq}
import scala.io.Source

/**
 * Created by nikita on 21.01.15.
 */
object PeptideSeq {
  object MassTable{
    private val massTable =  Map(
          'G' -> 57,
          'A' -> 71,
          'S' -> 87,
          'P' -> 97,
          'V' -> 99,
          'T' -> 101,
          'C' -> 103,
          'I' -> 113,
          'L' -> 113,
          'N' -> 114,
          'D' -> 115,
          'K' -> 128,
          'Q' -> 128,
          'E' -> 129,
          'M' -> 131,
          'H' -> 137,
          'F' -> 147,
          'R' -> 156,
          'Y' -> 163,
          'W' -> 186)
    private val distinctMasses = massTable.values.toList.distinct.sorted
    def masses = distinctMasses
    def apply(a:Char) = massTable(a)
  }

  type Peptide = List[Int]
  type Spectrum = Seq[Int]
  val EmptyPeptide = Nil

  def peptideFromString(peptideString:String):Peptide = peptideString.toList map (MassTable(_))

  def spectrumFromString(string: String):Spectrum = string split "\\s+" map (_.toInt)

  def expand(peptide:Peptide):List[Peptide] = {
    MassTable.masses map (m => m::peptide)
  }

  def expand(peptide: Peptide, alphapbet:Seq[Int]):Seq[Peptide] = {
    alphapbet map (_::peptide)
  }

  def linearScore(peptide:Peptide, spectrum: Spectrum):Int = {
    val peptideSpectrum = linearSpectrum(peptide)
    peptideSpectrum.length - (peptideSpectrum diff spectrum).length + 1
  }

  def cyclicScore(peptide:Peptide, spectrum: Spectrum):Int = {
    val peptideSpectrum = cyclicSpectrum(peptide)
    peptideSpectrum.length - (peptideSpectrum diff spectrum).length
  }

  /*
    Warning: returns spectrum without first "0" element
   */
  def linearSpectrum(peptide:Peptide):Spectrum = {
    val prefixMasses = peptide.foldLeft(List(0))((acc, v) => (acc.head+v)::acc ).reverse

    for (i <- 0 until peptide.size;
         j <- i + 1 to peptide.size
    ) yield prefixMasses(j) - prefixMasses(i)
  }

  def cyclicSpectrum(peptide:Peptide):Spectrum = {
    val peptideMass = peptide.sum
    val prefixMasses = peptide.foldLeft(List(0))((acc, v) => (acc.head+v)::acc ).reverse

    val res = mutable.ArrayBuffer(0)
    for(i <- 0 until peptide.size;
        j <- i + 1 to peptide.size){

      val subPeptideMass = prefixMasses(j) - prefixMasses(i)
      res += subPeptideMass
      if(i > 0 && j < peptide.size){
        res += peptideMass - subPeptideMass
      }
    }
    res.toSeq
  }
}
