package rosalind.util

/**
 * Created by nikita on 29.12.14.
 */
import Numeric.Implicits._
object TupleUtils {
  implicit def TupleUtilsExpention[A : Numeric, B : Numeric](t: (A, B)) = new {
    def + (p: (A, B)) = (p._1 + t._1, p._2 + t._2)
  }

}
