

import scala.collection.mutable.ListBuffer
object foo {

  val l = List(9, 8, 7, 6, 5, 5, 6)
  l.sliding(2).dropWhile{
    case List(x, y) => x > y
  }.next()
}