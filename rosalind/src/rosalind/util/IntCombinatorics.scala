package rosalind.util

object IntCombinatorics {

  def factorial(num: Int): BigDecimal = {
    (1 to num).map(x => BigDecimal.valueOf(x)).foldLeft(BigDecimal.valueOf(1)) ((a,b) => (a * b))
  }
  
  def choose(from:Int, count:Int) = {
    factorial(from)/(factorial(count)*factorial(from - count))  
  }
  
  implicit def facInt(n:Int) = new {
    def ! = IntCombinatorics.factorial(n)
    def factorial = IntCombinatorics.factorial(n)
    def choose(k:Int) = IntCombinatorics.choose(n, k)
  }
  

}