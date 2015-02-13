package rosalind.assignments.stepic.chapter3

import scala.io.Source

/**
 * Created by nikita on 28.01.15.
 */
object ste3_3_8 {
  def log2(value:Double) = Math.log(value)/Math.log(2)

  def entropy(values:Iterable[Double]):Double = {
    values.foldLeft(0d)((acc, v) => acc - v * log2(v))
  }

  def matrixEntropy(matrix:Seq[Seq[String]]):Double = {
    (matrix.transpose map { row =>
      val rowlen = row.size.toDouble
      row groupBy(_.toUpperCase) map (t => t._2.size/rowlen)
    } map entropy).sum
  }

  def main(args: Array[String]) {
    val data = Source.fromFile("./data/ste_3_3_8.txt").getLines().toList
    val matrix = data map (x =>x.split("\\s+").toList)

    println(matrixEntropy(matrix))
  }
}
