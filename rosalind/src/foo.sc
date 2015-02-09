

import scala.collection.mutable.ListBuffer
object foo {
  val l = 1 to 10
  (0 /: l )((acc, v) => acc + v)

  (0 /: l.zipWithIndex){case (acc, (v, i)) => acc + v}

  l map (_+1) toList;
  l map (_+1)

}