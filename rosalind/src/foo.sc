

import scala.collection.mutable.ListBuffer
object foo {
  val l = List((1, 1), (2, 3), (4, 1), (5, 3), (6, 3))
  l maxBy (_._2)
}