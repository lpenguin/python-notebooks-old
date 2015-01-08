package rosalind.assignments.stepic

import scala.io.Source
import scala.collection._
import rosalind.util.StringUtils._
import rosalind.util.WeightTable
/**
 * Created by nikita on 07.01.15.
 */
object ros2c {
  def main(args: Array[String]) {
    val peptide = Source.fromFile("./data/dataset_98_4.txt").mkString.trim
//    val peptide = Source.fromFile("./data/rosalind_2c.txt").mkString.trim
//    val peptide = Source.fromFile("./data/stepic_ros2c_sample.txt").mkString

    def obviousLinearSpectrum(peptide:String):Seq[Int] = {
      1 to peptide.size flatMap { size =>
        peptide slidingFast size map WeightTable.apply
      }
    }
//    println("0 " + (obviousLinearSpectrum(peptide).sorted mkString " "))

    def linearSpectrum(peptide:String):Seq[Int] = {
      val prefixMasses = Array.fill(peptide.size + 1)(0)
      for((char, i) <- peptide.zipWithIndex){
        prefixMasses(i + 1) = prefixMasses(i) + WeightTable(char).toInt
      }

      val res = mutable.ArrayBuffer(0)
      for(i <- 0 until peptide.size;
          j <- i + 1 to peptide.size){
        res += prefixMasses(j) - prefixMasses(i)
      }
      res.toSeq
    }

//    println(linearSpectrum(peptide).sorted mkString " ")

    def cyclicSpectrum(peptide:String):Seq[Int] = {
      val peptideMass = WeightTable(peptide)
      val prefixMasses = Array.fill(peptide.size + 1)(0)
      for((char, i) <- peptide.zipWithIndex){
        prefixMasses(i + 1) = prefixMasses(i) + WeightTable(char).toInt
      }

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

    println(cyclicSpectrum(peptide).sorted mkString " ")
  }
}
