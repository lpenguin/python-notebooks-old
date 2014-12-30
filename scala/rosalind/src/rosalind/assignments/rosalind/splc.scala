package rosalind.assignments.rosalind

import rosalind.util.{CodonTable, FastaReader}

/**
 * Created by nikita on 29.12.14.
 * http://rosalind.info/problems/splc/
 */
object splc {
  def main(args: Array[String]) {
    val data = FastaReader.fromData("rosalind_splc") map (_.value)
    val searched = data.head
    val subs = data.tail

    def removeSubs(searched:String, subStrings:List[String]):String = subStrings match {
      case Nil => searched
      case x::xs => removeSubs(searched.replaceFirst(x, ""), xs)
    }

    val mDna = removeSubs(searched, subs)
    println(CodonTable.translateGeneDna(mDna.toStream).mkString dropRight 1)
  }
}
