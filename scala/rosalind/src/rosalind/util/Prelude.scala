package rosalind.util

import scala.Predef.{println => predPrintln}
/**
 * Created by nikita on 22.01.15.
 */
object Prelude {
  implicit class ListUtilsExtension[A](val items: Iterable[A]) {
    def allMaxBy[B](key: (A) => B)(implicit cmp: Ordering[B]): List[A] = {
      items.foldLeft(List[A]()) { (acc, v) =>
        acc.headOption match {
          case None => v :: Nil
          case Some(head) =>

            val c = cmp.compare(key(head), key(v))
            if (c == 0) {
              v :: acc
            } else if (c < 0) {
              v :: Nil
            } else {
              acc
            }
        }
      }
    }
  }

  implicit def PrintedValue[A](a:A) = new {
    def println = predPrintln(a)
  }

  implicit def PrintedList[B, A <: Iterable[B]](a:A) = new {
    def println = predPrintln(a.mkString(", "))
  }

  implicit def ExpandedSeq[A](seq: Seq[A]) = new {
    def groupOrdered[K](f: A => K):List[(K, List[A])] = groupOrderedSeq(seq)(f)
  }


  def filterMax[A, K](seq: List[A])(f: A => K)(implicit cmp:Ordering[K]) = {
    var maxVal:Option[K] = None
    var maxList:List[A] = Nil
    for(item <- seq){
      if(maxVal == None) {
        maxVal = Some(f(item))
        maxList = List(item)
      }else{
        val k = f(item)
        val c = cmp.compare(maxVal.get, k)
        if(c < 0){
          maxVal = Some(k)
          maxList = List(item)
        }else if(c == 0){
          maxList = item :: maxList
        }

      }
    }
    maxList
  }
  def groupOrderedSeq[A, K](values:Seq[A])(f: A => K) = {
    def emptyAcc = List[(K, List[A])]()
    values.foldLeft(emptyAcc){ (acc:List[(K, List[A])], value:A) =>
      acc match {
        case Nil => List((f(value), List(value)))
        case (k, vals)::xs if k == f(value) => (k, value::vals) ::xs
        case _=> (f(value), List(value))::acc
      }
    }
  }

  implicit def StringUtilsExtension(s:String) = new {
    def slidingFast(windowLen:Int):Iterator[String] = StringUtils.slidingFast(s, windowLen)
  }

}
