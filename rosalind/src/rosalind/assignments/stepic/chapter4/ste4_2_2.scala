package rosalind.assignments.stepic.chapter4

import rosalind.assignments.common.Bioinformatics
import rosalind.assignments.common.Bioinformatics.{Kmer, Dna}
import rosalind.util.Extractor.Int
import rosalind.util.Prelude

/**
 * Created by nikitaprianichnikov on 13.02.15.
 */
object ste4_2_2 {
  def kmerComposition(k:Int, dna:Dna):Seq [Kmer] = {
    (dna sliding k).toList sortBy (_.mkString)
  }

  def main(args: Array[String]) {
    val data = Prelude.fromData("dataset_197_3")
    data match {
      case Int(k)::dna::Nil =>
        kmerComposition(k, Bioinformatics.fromString(dna)) map (_.mkString) foreach println
    }
  }
}
