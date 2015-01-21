package rosalind.assignments.stepic

import rosalind.util.Profiler
import rosalind.util.StringUtils._

import scala.io.Source

/**
 * Created by nikita on 04.01.15.
 */
object approx_pattern_match {
  def main(args: Array[String]) {
    {
      /*
      https://stepic.org/lesson/Some-Hidden-Messages-are-More-Elusive-than-Others-9/step/4?course=Bioinformatics-Algorithms&unit=407
       */
      val data = Source.fromFile("./data/stepic_approx_pattern_match_1.txt").getLines().toList
      val List(pattern, text, mismatchCount) = data

      val res = (text slidingFast pattern.size toStream).zipWithIndex filter (x => (x._1 hamming pattern) <= mismatchCount.toInt) map (_._2)
      println(res.mkString(" "))
    }

    {
      /*
      https://stepic.org/lesson/Some-Hidden-Messages-are-More-Elusive-than-Others-9/step/5?course=Bioinformatics-Algorithms&unit=407
       */

      val text = "AACAAGCTGATAAACATTTAAAGAG"
      val pattern = "AAAAA"
      val mismatchCount = 2

      val res = (text slidingFast pattern.size toStream).zipWithIndex count (x => (x._1 hamming pattern) <= mismatchCount)
      println(res)
    }

    {
      /*
      https://stepic.org/lesson/Some-Hidden-Messages-are-More-Elusive-than-Others-9/step/4?course=Bioinformatics-Algorithms&unit=407
       */

      val data = Source.fromFile("./data/stepic_approx_pattern_match_count_1.txt").getLines().toList
      val List(text, pattern, mismatchCountStr) = data
      val mismatchCount = mismatchCountStr.toInt
      val res = (text slidingFast pattern.size toStream).zipWithIndex count (x => (x._1 hamming pattern) <= mismatchCount)
      println(res)
    }

    {
      import rosalind.util.FrequentWords._
      val List(text, params) = Source.fromFile("./data/frequent_words_mismatch_data_1.txt").getLines().toList
      val List(k, d) = params split "\\s+" map (_.toInt) toList

      println(text.size)
      println("Got: "+Profiler.profile({
        0 to Math.pow(4, k).toInt foreach { i =>
          val p = numberToPattern(i, k)
          text slidingFast k count (x => (x hamming p) < d)
        }
      }))
    }
  }
}
