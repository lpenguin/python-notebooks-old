package rosalind.assignments.stepic.common
import rosalind.util.Prelude._

import scala.collection.immutable.Stream.Empty

/**
 * Created by nikita on 29.01.15.
 */
object Motif {
  val dictStream = (dict map (_::Nil)).toStream
  def enumeratePatterns(n:Int):Stream[CharList] = n match {
    case 1 => dictStream
    case _ =>
      val subPatterns = enumeratePatterns(n - 1)
      subPatterns flatMap (p => dict map (_::p))
  }
}
