package rosalind.assignments.stepic.chapter3

import rosalind.util.Prelude._

import scala.collection.mutable
import scala.io.Source
/**
 * Created by nikita on 28.01.15.
 */
object ste3_2_6 {
  type Pattern = List[Char]
  val dictionary = "ACGT".toList
  val unitPatternList = dictionary map (_ :: Nil)

  def hamming(pattern1:Seq[Char], pattern2:Seq[Char]):Int = (pattern1 zip pattern2) count (t => t._1 != t._2)

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

  def isPatternOccurs(dna:String, pattern: Pattern, d:Int):Boolean = {
    dna slidingFast pattern.size exists (subPattern => hamming(subPattern, pattern) <= d)
  }

  def motifEnumeration(dnaList:Seq[String], k:Int, d:Int):Seq[Pattern] = {
    var result = mutable.Set[Pattern]()

    for(dna <- dnaList){
      for(kmer <- dna.slidingFast(k)){
        val kmerl = kmer.toList
        for(kmermut <- neighbors(kmerl, d)){
          if(dnaList forall (isPatternOccurs(_, kmermut, d))){
            result += kmermut
          }
        }
      }
    }
    result.toSeq
  }

  def main(args: Array[String]) {
    val data = Source.fromFile("./data/dataset_156_7.txt").getLines().toList
    val List(k, d) = (data.head split "\\s+" map (_.toInt)).toList
    val dnaList = data.tail

    val result = motifEnumeration(dnaList, k, d)
    println((result map (_.mkString)).mkString(" "))
  }
}
