package rosalind.assignments.stepic

import rosalind.util.PatternMatch._
import scala.io.Source

/**
 * Created by nikita on 30.12.14.
 */
object clump_1d {
  def main(args: Array[String]) {
    val List(genome, values) = Source.fromFile("./data/stepic_clump_1.txt").getLines().toList
    val Array(k, l, t) = values.split("\\s+") map (_.toInt)

    def patternCount(text:String, pattern:String) = {
      (0 /: (0 to text.size - pattern.size)){(acc, i) =>
        acc + (if(text.substring(i, i + pattern.size) == pattern) 1 else 0)
      }
    }

    def frequentWordsNaive(text:String, k:Int, t:Int):Seq[String] = {
      val count = new collection.mutable.HashMap[Int, Int]()

      for(i <- 0 to text.size - k){
        val pattern = text.substring(i, i + k)
        count(i) = patternCount(text, pattern)
      }

      val frequents = new collection.mutable.LinkedHashSet[String]()
      for(i <- 0 to text.size - k){
        if(count(i) >= t){
          frequents += text.substring(i, i + k)
        }
      }

      frequents.toSeq
    }

    def findClumps(text:String, k:Int, l:Int, t:Int) = {
      def processWindow(window:String):Seq[String] = {
//        val fa = computeFrequencyArray(window, k)
//        (fa.zipWithIndex filter (_._1 >= t)) map {x => numberToPattern(x._2, k)}
        frequentWordsNaive(window, k, t)
      }
      (text sliding l).zipWithIndex flatMap {
        x =>
          if(x._2 % 500 == 0){ println(s"Processed ${x._2} symbols")}
          processWindow(x._1)} toSet
    }
    //FIXME Works too slow...
    println(s"Processing ${genome.size} chars string")
    findClumps(genome, k, l, t) foreach println
  }
}
