import rosalind.util.Profiler

import scala.collection.immutable.Stream.Empty

def posCombinations(length:Int, d:Int):List[List[(Int, Char)]] = {
  val symCombinations = ("acgt" * d combinations d).toList
  symCombinations flatMap (strComb => {
    (0 to length - 1 combinations d).toList map {
      indexComb => indexComb.toList zip strComb
    }
  })
}


def insert(target:Array[Char], pos:List[(Int, Char)]):String = pos match {
  case Nil => target.mkString
  case x::xs =>
    target(x._1) = x._2
    insert(target, xs)
}

//"abc" zip List(2, 4, 7)
//insert("01234567", "abc" zip List(2, 4, 7))
def mutations(str:String, d:Int):List[String] = {
  val strA = str.toCharArray
  val pc = posCombinations(str.length, d)
  pc map { comb =>
    insert(strA, comb)
  }

}
val q = " " * 12

Profiler.profile({
  mutations(q, 3).size

})

val qq = "1234"
"1234" diff "q23e"
