package rosalind.util

/**
 * Created by nikita on 30.12.14.
 */
object PatternMatch {
  val dict = "ACGT"
  val symbolsDigit = dict.zipWithIndex.toMap
  val digitSymbols =symbolsDigit map (f => f.swap)

  def patternToNumber(str:String):Int = {
    (0 /: str.reverse.zipWithIndex){(acc, v) => acc + symbolsDigit(v._1) * Math.pow(dict.size, v._2).toInt}
  }

  def numberToPattern(number:Int, patterLength:Int):String = {
    def iter(number: Int): List[Char] = number match {
      case 0 => Nil
      case _ =>
        val digit = number % dict.size
        digitSymbols(digit) :: iter(number / dict.size)
    }
    val unboundStr = iter(number).reverse.mkString
    unboundStr
//    dict.head.toString * (patterLength - unboundStr.size) + unboundStr
  }

  def computeFrequencyArray(text:String, k:Int):Array[Int] = {
    val freqArray = Array.fill(Math.pow(dict.size, k).toInt)(0)
    text sliding k foreach { str =>
      val num = patternToNumber(str)
      freqArray(num) += 1
    }
    freqArray
  }

  def frequentWords(text:String, k:Int):Seq[String] = {
    val freqArray = computeFrequencyArray(text, k)
    val max = freqArray.max
    freqArray.zipWithIndex filter (_._1 == max) map (f => numberToPattern(f._2, k))
  }
}
