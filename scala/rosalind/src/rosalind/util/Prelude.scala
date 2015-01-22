package rosalind.util

import scala.Predef.{println => predPrintln}
/**
 * Created by nikita on 22.01.15.
 */
object Prelude {
  implicit def PrintedValue[A](a:A) = new {
    def println = predPrintln(a)
  }

  implicit def PrintedList[B, A <: Iterable[B]](a:A) = new {
    def println = predPrintln(a.mkString(", "))
  }
}
