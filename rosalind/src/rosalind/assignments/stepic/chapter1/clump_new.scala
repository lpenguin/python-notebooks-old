package rosalind.assignments.stepic.chapter1

import rosalind.util.FrequentWords._
import rosalind.util.Profiler

import scala.io.Source

/**
 * Created by nikita on 04.01.15.
 */
object clump_new {
  def main(args: Array[String]) {

    val genome = Source.fromFile("./data/e_coli.txt").mkString

    val (k, l, t) = (14, 500, 3)//ints split "\\s+" map (_.toInt) toList

    def clumpFindingNaive(genome:String, k:Int, t:Int, l:Int):Seq[String] = {
      val patternNumbers = collection.mutable.Set[Int]()
      genome sliding l foreach (text => {
        val fa = computeFrequencyArray(text, k)
        patternNumbers ++= fa.zipWithIndex filter (_._1 >= t) map (_._2)
      })
      patternNumbers map ( numberToPattern(_, k)) toSeq
    }

    def clumpFindingIterative(genome:String, k:Int, t:Int, l:Int):Seq[String] = {
      val patternNumbers = collection.mutable.Set[Int]()
      val firstString = genome.take(l)
      val fa = computeFrequencyArray(firstString, k)
      patternNumbers ++= fa filter (_ >= t)
      var firstNumber = patternToNumber(firstString.take(k))

      1 to (genome.size - l -1) foreach { i =>
        val text = genome.substring(i, i+l)
        fa(firstNumber) = Math.max(0, fa(firstNumber) - 1)
        val lastNumber = patternToNumber(text.takeRight(k))
        fa(lastNumber) += 1
        if(fa(lastNumber) >= t){
          patternNumbers += lastNumber
        }
        firstNumber = patternToNumber(text.take(k))
      }
//      genome.drop(1) sliding l foreach (text => {
//        fa(firstNumber) = Math.max(0, fa(firstNumber) - 1)
//        val lastNumber = patternToNumber(text.takeRight(k))
//        fa(lastNumber) += 1
//        if(fa(lastNumber) >= t){
//          patternNumbers += lastNumber
//        }
//        firstNumber = patternToNumber(text.take(k))
//      })
      patternNumbers map ( numberToPattern(_, k)) toSeq
    }

    def clumpFindingIterativeSliding(genome:String, k:Int, t:Int, l:Int):Seq[String] = {
      val patternNumbers = collection.mutable.Set[Int]()
      val firstString = genome.take(l)
      val fa = computeFrequencyArray(firstString, k)
      patternNumbers ++= fa filter (_ >= t)
      var firstNumber = patternToNumber(firstString.take(k))

      genome.drop(1) sliding l foreach (text => {
        fa(firstNumber) = Math.max(0, fa(firstNumber) - 1)
        val lastNumber = patternToNumber(text.takeRight(k))
        fa(lastNumber) += 1
        if(fa(lastNumber) >= t){
          patternNumbers += lastNumber
        }
        firstNumber = patternToNumber(text.take(k))
      })
      patternNumbers map ( numberToPattern(_, k)) toSeq
    }

    println("Got text: "+genome.size+" bytes")
    println("Iterative: "+Profiler.profile({
      val res = clumpFindingIterative(genome, k, t, l)
//      res foreach println
      println(s"Total: ${res.size}")
    }))

  }
}
