package rosalind.assignments.stepic.chapter3

//import rosalind.assignments.common.Motif.ProfileMatrix
import rosalind.assignments.common.Bioinformatics.{Dna, Kmer, fromString}
import rosalind.assignments.common.Motif._
import rosalind.util.Extractor.{Extract, Int}
import rosalind.util.Prelude
import rosalind.util.Prelude.Task

/**
 * Created by nikitaprianichnikov on 09.02.15.
 */
object ste3_5_9 {
  object GreedMotifSearch extends Task{
    def greedMotifSearch(k:Int, t:Int, dnas:List[Dna]):List[Kmer] = dnas match {
      case firstDna::restDnas =>
        firstDna sliding k map {
          motif =>
            restDnas.foldLeft(motif::Nil){
              (motifs, dna) =>
                val profileMatrix = buildProfileMatrixLaplass(motifs)
                profileMostProbableKmer(profileMatrix, dna)::motifs
            }.reverse
        } minBy score
    }

    override def run(data: List[String]): Seq[String] = {
      data match {
        case Extract(Int(k), Int(t))::dnas =>
          greedMotifSearch(k, t, dnas map fromString) map (_.mkString)
      }
    }
  }


  def main(args: Array[String]) {
    GreedMotifSearch.run(Prelude.fromData("dataset_160_9")) foreach println
  }
}
