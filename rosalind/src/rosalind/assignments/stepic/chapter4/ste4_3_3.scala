package rosalind.assignments.stepic.chapter4

import rosalind.assignments.common.Bioinformatics.{Nucleotides, fromString}
import rosalind.util.Prelude

/**
 * Created by nikitaprianichnikov on 20.02.15.
 */
object ste4_3_3 {
  def simpleStringReconstruction(nucs:List[Nucleotides]):Nucleotides = {
    nucs.head ++ (nucs.tail map (_.last))
  }

  def main(args: Array[String]) {
    Prelude.fromData("dataset_198_3") match {
      case nucs =>
        println(simpleStringReconstruction(nucs map fromString).mkString)
    }
  }
}
