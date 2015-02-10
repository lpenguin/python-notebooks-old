import scala.util.Random
object foo {
  val list = List(3, 2, 4, 1)
  val (l,r) = list.splitAt(3)
//  val q = l.foldLeft((0, 0f)::Nil){
//    case (p@(index, sum)::xs, item) => (index + 1, sum + item)::p
//  }

//  def genRandom(probs:Iterator[Float]):Int = {
//    val totalSum = probs.sum
//    val v = Random.nextFloat() * totalSum
//
//  }

}