import scala.collection.immutable.::
import scala.util.Random
object foo {
  val l1 = List(1, 2, 3, 4, 5)
  val l2 = List(0, 1, 2, 3, 4)
  l1 zip l2.tail forall {case (a, b) => a == b}



}