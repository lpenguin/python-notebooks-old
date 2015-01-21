package rosalind.assignments.stepic

import java.lang.Comparable

import rosalind.util.Profiler
import rosalind.util.ListUtils._
import rosalind.util.ListCombinatorics._
import rosalind.util.StringUtils._
import scala.collection.mutable
import scala.io.Source

/**
 * Created by nikita on 06.01.15.
 */
object ros1g {
  /*
  http://rosalind.info/problems/1g/
  https://stepic.org/lesson/Some-Hidden-Messages-are-More-Elusive-than-Others-9/step/7?course=Bioinformatics-Algorithms&unit=407
   */

  def main(args: Array[String]) {
//    val List(text, params) = Source.fromFile("./data/stepic_ros1g_2.txt").getLines().toList
    val List(text, params) = Source.fromFile("./data/stepic_ros1g_stepic_1.txt").getLines().toList
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
        }
      }
      mutationsCounts allMaxBy (_._2) map (_._1)
    }

    println(calcCountsShareDict(text, k, d) mkString " ")
  }
}
