package rosalind.assignments.stepic

//import rosalind.assignments.stepic.common.Motif.ProfileMatrix
import rosalind.assignments.stepic.common.Bioinformatics.{Kmer, Dna, Nucleotides, fromString}
import rosalind.assignments.stepic.common.Motif.ProfileMatrix
import rosalind.util.Extractor.{Int, Extract}
import rosalind.assignments.stepic.common.Motif._
import scala.io.Source

/**
 * Created by nikitaprianichnikov on 09.02.15.
 */
object ste3_5_9 {


  def greedMotifSearch(k:Int, t:Int, dnas:List[Dna]):List[Kmer] = dnas match {
    case firstDna::restDnas =>
      firstDna sliding k map {
        motif =>
          var motifs = List(motif)
          for(i <- 0 until t - 1){
            val profileMatrix = buildProfileMatrixLaplass(motifs)
            motifs = profileMostProbableKmer(profileMatrix, restDnas(i))::motifs
          }
          motifs.reverse
      } minBy score
  }

  def main(args: Array[String]) {
    val data = Source.fromFile("./data/dataset_160_9.txt").getLines().toList
    data match {
      case Extract(Int(k), Int(t))::dnas =>
        val res = greedMotifSearch(k, t, dnas map fromString)
        for(motif <- res){
          println(motif.mkString)
        }
    }
  }
}
