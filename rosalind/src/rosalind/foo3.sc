import rosalind.util.ListCombinatorics._
import rosalind.util.Profiler
import rosalind.util.StringUtils._


import scala.collection.mutable
import scala.util.Random

permutationsRep(List(1, 2, 3, 4), 5).distinct.size
Math.pow(4, 5)




//def insert(str:String, pos:Seq[(Int, Char)]):String = {
//  val builder = new mutable.StringBuilder(str)
//  for((index, char) <- pos){
//    builder.replace(index, index + 1, char.toString)
//  }
//  builder.toString()
//}
//
//def prof(name:String)(code:(String, Seq[(Int, Char)])=>Any) = {
//  val avgTime = Profiler.profileTimes(10000){
//    val str = "q" * 256
//
//    val positions = 0 to str.size/4 map (_ => Random.nextInt(str.size))
//    val chars = ("e" * str.size).toList
//    code(str, positions zip chars)
//  }
//  avgTime
//}
mutations(letterDicts("ACTG".toList), 1)("ACC").toList
//prof("patch")(insertPatch)
//prof("builder")(insert)
//prof("array")(insert)
//def mutations(str:String, d:Int):Seq[String] = {
//  val possibleIndexes = 0 until str.size combinations d
//
//}
//def insertPatch(str:String, pos:Seq[(Int, Char)]):String = {
//  var res = str
//  for((index, char) <- pos){
//    res = str.patch(index, char::Nil, 1)
//  }
//  res
//}

//def insert(str:String, pos:Seq[(Int, Char)]):String = {
//  val array = str.toCharArray
//  for((index, char) <- pos){
//    array(index) = char
//  }
//  array.mkString
//}