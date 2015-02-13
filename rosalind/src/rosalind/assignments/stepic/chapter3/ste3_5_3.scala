package rosalind.assignments.stepic.chapter3

import rosalind.util.Prelude._

import scala.io.Source
/**
 * Created by nikita on 30.01.15.
 */
object ste3_5_3 {
  def profileMostProbableKmer(dna:String, k:Int, probs:Seq[Seq[Float]]):String = {
    val indexMap = Map(0 -> 'A', 1 -> 'C', 2 -> 'G', 3 -> 'T')
    val profileMatrix = probs.transpose map (col => col.zipWithIndex map (t => indexMap(t._2) -> t._1) toMap)

    def kmerProb(kmer:String):Float = {
      (1f /: (kmer zip profileMatrix))((acc, t) => acc * t._2(t._1))
    }

    dna slidingFast k maxBy kmerProb
  }

  def main(args: Array[String]) {
    val data = Source.fromFile("./data/dataset_159_3.txt").getLines().toList
    val dna = data.head
    val k = data.tail.head.toInt
    val probs = data.drop(2) map (_ split "\\s+" map (_.toFloat) toList)

    println(profileMostProbableKmer(dna, k, probs))
  }
}
