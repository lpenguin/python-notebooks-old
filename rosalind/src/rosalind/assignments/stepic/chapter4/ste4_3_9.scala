package rosalind.assignments.stepic.chapter4

import rosalind.assignments.common.Bioinformatics.{Nucleotides, fromString}
import rosalind.util.Prelude

/**
 * Created by nikitaprianichnikov on 20.02.15.
 */
object ste4_3_9 {
  def adjListGraph(nucs:List[Nucleotides]):Seq[(Nucleotides, Nucleotides)] = {
    for {
      nuc1 <- nucs
      nuc2 <- nucs
      if nuc1.tail zip nuc2 forall {case (a, b) => a == b}
    } yield (nuc1, nuc2)
  }

  def main(args: Array[String]) {
    Prelude.fromData("dataset_198_9") match {
      case nucs =>
        val adjList = adjListGraph(nucs map fromString)
        for((from, to) <- adjList){
          println(s"${from.mkString} -> ${to.mkString}")
        }
    }
  }
}
