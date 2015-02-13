package rosalind.assignments.stepic.chapter2

import rosalind.util.WeightTable

import scala.collection._
import scala.collection.mutable.ListBuffer
import scala.io.Source
/**
 * Created by nikita on 09.01.15.
 */
object ros2e {
  def expand(peptide:List[Int]):Iterable[List[Int]] = {
    WeightTable.masses map (m => m :: peptide)
  }

  def linearSpectrum(peptide:List[Int]):Seq[Int] = {
    (1 to peptide.size) flatMap (i => (peptide sliding i) map (_.sum))
  }

  def cyclicSpectrum(peptide:List[Int]) = {
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

  def peptideMass(peptide:List[Int]) = peptide.sum



  def isPeptideConsistent(peptide:List[Int], spectrum:List[Int]):Boolean = {
    val elems = mutable.ListBuffer[Int]() ++ spectrum
    for(p <- linearSpectrum(peptide)){
      if(!elems.contains(p)){
        return false
      }
      elems -= p
    }
    true
  }

  def isPeptideCandidate(peptide:List[Int], spectrum:List[Int]):Boolean = {
    val elems = mutable.ListBuffer[Int]() ++ spectrum
    for(p <- cyclicSpectrum(peptide)){
      if(!elems.contains(p)){
        return false
      }
      elems -= p
    }
    elems.isEmpty
  }

  def main(args: Array[String]) {
    val spectrum = Source.fromFile("./data/dataset_100_5.txt").mkString.trim.split("\\s+").reverse.toList map (_.toInt)
//    val spectrum = Source.fromFile("./data/rosalind_2e.txt").mkString.trim.split("\\s+").reverse.toList map (_.toInt)
//    val spectrum = Source.fromFile("./data/stepic_ros2e_sample.txt").mkString.trim.split("\\s+").reverse.toList map (_.toInt)
    var peptides = mutable.ListBuffer[List[Int]]() += List[Int]()
    val parentMass = spectrum.head
    println(spectrum)
    println(cyclicSpectrum(List(1, 2, 3)))
    val results = ListBuffer[List[Int]]()
    while(peptides.nonEmpty){
      peptides = peptides flatMap (p => expand(p))
      println("Current peptideList: "+peptides)
      for(peptide <- peptides){
//        println(s"${peptideMass(peptide)} - $parentMass")
        if(peptideMass(peptide) == parentMass){
          println("Got result candidate: "+peptide+" "+cyclicSpectrum(peptide).distinct.sorted+" "+spectrum)
          if(isPeptideCandidate(peptide, spectrum)){
            println("Spectrum matches: "+peptide)
            results += peptide
          }
          peptides -= peptide
        }else{
//          println(s"const: $peptide: ${linearSpectrum(peptide).toList} $spectrum")
          if(!isPeptideConsistent(peptide, spectrum)){
            peptides -= peptide
          }
        }
      }
    }

    println(results map (p => p mkString "-") mkString " ")
  }
}
