package rosalind.assignments.stepic

import scala.io.Source

/**
 * Created by nikita on 28.01.15.
 */
object ste1_16 {
  def main(args: Array[String]) {
    type Pattern = List[Char]

    val dictionary = "ACGT".toList
    val unitPatternList = dictionary map (_ :: Nil)

    def hamming(pattern1:Pattern, pattern2:Pattern):Int = (pattern1 zip pattern2) count (t => t._1 != t._2)

    def neighbors(pattern:Pattern, d:Int):List[Pattern] = pattern match {
      case x::Nil => unitPatternList
      case first::suffix =>
        val patterns = neighbors(suffix, d)
        patterns flatMap { pattern =>
          hamming(pattern, suffix) match {
            case h if h == d => List(first::pattern)
            case _ => dictionary map (_::pattern)
          }
        }
    }

//    val data = Source.fromFile("./data/dataset_3014_3.txt").getLines().toList
//    val List(pattern, dStr) = data
//
//    val d = dStr.toInt
    val pattern = "ACG".toList
    val d = 2
    println("Heighbors of: "+pattern)
    for(str <- neighbors(pattern.toList, d)){
      println(str.mkString)
    }
  }
}
