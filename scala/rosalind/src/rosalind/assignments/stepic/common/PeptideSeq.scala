package rosalind.assignments.stepic.common

import scala.collection.{mutable, Seq}
import scala.io.Source

/**
 * Created by nikita on 21.01.15.
 */
object PeptideSeq {
  object MassTable{
    def readMasses() = {
      val data = Source.fromFile("./data/integer_mass_table.txt").getLines()
      data map (line => {
        val Array(key, mass) = line split "\\s+"
        (key.head, mass.toInt)
      }) toMap
    }
    private val massTable = readMasses()
    def masses = massTable.values
  }

  type Peptide = List[Int]
  type Spectrum = List[Int]

  def expand(peptide:Peptide):Iterable[Peptide] = {
    MassTable.masses map (m => m::peptide)
  }

  def score(peptide:Peptide, spectrum: Spectrum):Int = {
    val peptideSpectrum = cyclicSpectrum(peptide)
    peptideSpectrum.length - (peptideSpectrum diff spectrum).length
  }

  def linearSpectrum(peptide:Peptide):Seq[Int] = {
    val prefixMasses = peptide.foldLeft(List(0))((acc, v) => (acc.head+v)::acc ).reverse

    for(i <- 0 until peptide.size;
        j <- i + 1 to peptide.size
    ) yield prefixMasses(j) - prefixMasses(i)
  }

  def cyclicSpectrum(peptide:Peptide):Seq[Int] = {
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
