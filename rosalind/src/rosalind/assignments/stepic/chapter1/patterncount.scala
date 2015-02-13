package rosalind.assignments.stepic.chapter1

import scala.io.Source

/**
 * Created by nikita on 30.12.14.
 */
object patterncount {
  def main(args: Array[String]) {
    def patternCount(text:String, pattern:String) = {
      var count = 0
      for(i <- 0 to text.size - pattern.size){
        if(text.substring(i, i + pattern.size) == pattern){
          count += 1
        }
      }
      count
    }

    println(patternCount("GCGCG", "GCG"))


    def frequentWordsNaive(text:String, k:Int):Seq[String] = {
      val count = new collection.mutable.HashMap[Int, Int]()
      for(i <- 0 to text.size - k){
        val pattern = text.substring(i, i + k)
        count(i) = patternCount(text, pattern)
      }

      val maxCount = count.maxBy((t) => t._2)._2
      val frequents = new collection.mutable.LinkedHashSet[String]()
      for(i <- 0 to text.size - k){
        if(count(i) == maxCount){
          frequents += text.substring(i, i + k)
        }
      }

      frequents.toSeq
    }

    val List(data, count) = Source.fromFile("./data/stepic_fw_1.txt").getLines().toList
    frequentWordsNaive(data, count.toInt) foreach println

//    def frequentWords(text:String, k:Int):Seq[String] = {
//      val freqArray = computeFrequencyArray(text, k)
//      val max = freqArray.max
//      freqArray.zipWithIndex filter (_._1 == max) map (f => numberToPattern(f._2, k))
//    }
//    println(patternToNumber("ATGCAA"))
//    println(numberToPattern(912, 6))
//    println(numberToPattern(5437, 8))
//    println(computeFrequencyArray(data, count.toInt) mkString " ")
//    frequentWords(data, count.toInt) foreach println


  }
}
