package rosalind.util

object ListCombinatorics {
  def combinations[T](values:List[T], count:Int):List[List[T]] = count match {
    case 1 => values.map((x) => List(x)) 
    case _ => { 
      values match {
        case Nil => Nil
        case x::xs => combinations(xs, count - 1).map((variant) => x::variant) ++ combinations(xs, count)
      }
    }
  }


  def permutationsRep[T](dict: List[T], count:Int):List[List[T]] = count match {
    case 0 => dict map (List(_))
    case _ =>
      dict flatMap (x => permutationsRep(dict, count - 1) map ( x::_))
  }


  implicit def ListCombinatoricsExtention[T](l:List[T]) = new {
    def choose(k:Int) = ListCombinatorics.combinations(l, k)
    def permutationsRep(k:Int) = ListCombinatorics.permutationsRep(l, k)
  }
}