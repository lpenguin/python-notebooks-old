package rosalind.assignments.stepic.chapter1

import rosalind.util.ListUtils._
import rosalind.util.StringUtils._

import scala.collection.mutable
import scala.io.Source

/**
 * Created by nikita on 07.01.15.
 */
object ros1h {
  /*
  http://rosalind.info/problems/1h/
  https://stepic.org/lesson/Some-Hidden-Messages-are-More-Elusive-than-Others-9/step/8?course=Bioinformatics-Algorithms&unit=407
   */
  def main(args: Array[String]) {
    val List(text, params) = Source.fromFile("./data/ecoli_dna_ori.txt").getLines().toList
//    val List(text, params) = Source.fromFile("./data/stepic_ros1h_stepic1.txt").getLines().toList
    val List(k, d) = params split "\\s+" map (_.toInt) toList

    def calcCountsShareDict(text:String, k:Int, d:Int) = {
      def kmerMutations(str:String) = mutations(letterDicts("ACTG".toList), d)(str)

      // Initial fill
      val kmerCounts = mutable.Map[String, Int]().withDefaultValue(0)
      for(kmer <- text slidingFast k){
        kmerCounts(kmer) += 1
      }

      val mutationsCounts = mutable.Map[String, Int]().withDefaultValue(0)
      for((kmer, count) <- kmerCounts){
        for(mutation <- kmerMutations(kmer)){
          mutationsCounts(mutation) = mutationsCounts(mutation) + count
          val reverseComplement = mutation.reverseComplement
          mutationsCounts(reverseComplement) = mutationsCounts(reverseComplement) + count
        }
      }
      mutationsCounts allMaxBy (_._2)// map (_._1)
    }

//    println(calcCountsShareDict(text, k, d) mkString " ")
    val res = calcCountsShareDict(text, k, d)
    val s = "TTATCCACA"
    val sRev = s.reverseComplement

    val matched = text slidingFast k filter (_.hamming(s) <= 1)
    val matched2 = text slidingFast k filter (_.hamming(sRev) <= 1)

    def checkResults(results:List[(String, Int)]) = {
      for((s, count) <- results){
        val sRev = s.reverseComplement
        val matched = text slidingFast k filter (t => (t.hamming(s) <= d) || t.hamming(sRev) <= d)
        val ms = matched.size
        if(count != ms){
          println(s"Invalid result: $s $count vs ${ms}")
        }
      }
    }

    res foreach println
    checkResults(res)
  }
}
