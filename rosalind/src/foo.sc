import scala.collection.immutable.::
import scala.util.Random
object foo {
  val probs = List(4, 1, 2, 2)
  def takeAtSum(v:Float) = {
    probs.foldLeft((0, 0f)::Nil){
      case (p@(index, sum)::xs, item) => (index + 1, sum + item)::p
    } find {
      case (index, sum) => sum <= v
    } match {
      case Some((index, _)) => index
    }
  }

  for(v <- 0 to probs.sum){
    println(s"$v ${takeAtSum(v)}")
  }
//  val q = l.foldLeft((0, 0f)::Nil){
//    case (p@(index, sum)::xs, item) => (index + 1, sum + item)::p
//  }

//  def genRandom(probs:Iterator[Float]):Int = {
//    val totalSum = probs.sum
//    val v = Random.nextFloat() * totalSum
//
//  }

}