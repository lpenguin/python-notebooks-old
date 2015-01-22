package rosalind.util

import scala.collection.{Iterable, mutable, Seq}
import StringUtils._
/**
 * Created by nikita on 10.01.15.
 */
object Spectrum {
  def peptideMass(peptide:Seq[Int]) = peptide.sum

  def cyclicSpectrum(peptide:String):Seq[Int] = cyclicSpectrum(massesList(peptide))

  def cyclicSpectrum(peptide:Seq[Int]):Seq[Int] = {
    val totalMass = peptideMass(peptide)
    val prefixMasses = Array.fill(peptide.size + 1)(0)
    for((mass, i) <- peptide.zipWithIndex){
      prefixMasses(i + 1) = prefixMasses(i) + mass
    }

    val res = mutable.ArrayBuffer(0)
    for(i <- 0 until peptide.size;
        j <- i + 1 to peptide.size){
      val subPeptideMass = prefixMasses(j) - prefixMasses(i)
      res += subPeptideMass
      if(i > 0 && j < peptide.size){
        res += totalMass - subPeptideMass
      }
    }
    res.toList
  }

  def linearSpectrumObvious(peptide:String):Seq[Int] = linearSpectrum(massesList(peptide))

  def linearSpectrumObvious(peptide:Seq[Int]):Seq[Int] = {
    (1 to peptide.size) flatMap (i => (peptide sliding i) map (_.sum))
  }

  def linearSpectrum(peptide:String):Seq[Int] = linearSpectrum(massesList(peptide))

  def linearSpectrum(peptide:Seq[Int]):Seq[Int] = {
    val prefixMasses = Array.fill(peptide.size + 1)(0)
    for((v, i) <- peptide.zipWithIndex){
      prefixMasses(i + 1) = prefixMasses(i) + v
    }

    val res = mutable.ArrayBuffer(0)
    for(i <- 0 until peptide.size;
        j <- i + 1 to peptide.size){
      res += prefixMasses(j) - prefixMasses(i)
    }
    res.toSeq
  }

  def expand(peptide:List[Int]):Iterable[List[Int]] = {
    WeightTable.masses map (m => m :: peptide)
  }

  def massesList(peptide:String):Seq[Int] = peptide map WeightTable.apply map (_.toInt)

  def score(peptide:Seq[Int], spectrum:Seq[Int]):Int = {
    var score = 0
    val elems = mutable.ListBuffer[Int]() ++ spectrum
    for(p <- cyclicSpectrum(peptide)){
      if(elems.contains(p)){
        score += 1
      }
      elems -= p
    }
    score
  }

  def linearScore(peptide:Seq[Int], spectrum:Seq[Int]):Int = {
    var score = 0
    val elems = mutable.ListBuffer[Int]() ++ spectrum
    for(p <- linearSpectrum(peptide)){
      if(elems.contains(p)){
        score += 1
      }
      elems -= p
    }
    score
  }
}
