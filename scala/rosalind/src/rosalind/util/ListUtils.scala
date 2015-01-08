package rosalind.util

import com.sun.tools.javac.jvm.Items

/**
 * Created by nikita on 06.01.15.
 */
object ListUtils{
  implicit class ListUtilsExtension[A](val items: Iterable[A]){
    def allMaxBy[B](key: (A) => B)(implicit cmp: Ordering[B]):List[A] = {
      items.foldLeft(List[A]()){ (acc, v) =>
        acc.headOption match {
          case None => v::Nil
          case Some(head) =>

            val c = cmp.compare(key(head), key(v))
            if(c == 0){
              v::acc
            }else if(c < 0){
              v::Nil
            }else{
              acc
            }
        }
      }
    }
  }

}

