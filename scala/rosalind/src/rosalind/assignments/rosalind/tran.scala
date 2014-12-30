package rosalind.assignments.rosalind

import rosalind.util.FastaReader

/**
 * Created by nikita on 29.12.14.
 * rosalind.info/problems/tran/
 */
import rosalind.util.TupleUtils._
object tran {

  def main(args: Array[String]) {
    val List(s1, s2) = FastaReader.fromData("rosalind_tran") map (_.value)

    val AG = Set('A', 'G')
    val CT = Set('C', 'T')

    def mutType(c1:Char, c2:Char) = {
      if(c1 == c2) (0, 0)
      else {
        val s = Set(c1, c2)
        if(s == AG || s == CT) (1, 0) else (0, 1)
      }
    }

    val (trs, trv) = ((0, 0) /: (s1 zip s2)) {
      (z, i) => z + mutType(i._1, i._2)
    }

    println(trs/trv.toFloat)

  }
}
