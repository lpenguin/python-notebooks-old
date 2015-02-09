package rosalind.assignments.stepic

//import rosalind.assignments.stepic.common.Motif.ProfileMatrix
import rosalind.assignments.stepic.common.Bioinformatics.{Kmer, Dna, Nucleotides, fromString}
import rosalind.assignments.stepic.common.Motif.ProfileMatrix
import rosalind.util.Extractor.{Int, Extract}

import scala.io.Source

/**
 * Created by nikitaprianichnikov on 09.02.15.
 */
object ste3_5_9 {
  def buildProfileMatrix(motifs:Seq[Kmer]):ProfileMatrix =
    motifs.transpose map { col =>
      val colSize = (col.size + 4).toFloat
      (col groupBy identity map {
        x => (x._1,  (x._2.size + 1) / colSize)
      }).toMap.withDefaultValue(1f/colSize)
    }

  def profileMostProbableKmer(profileMatrix: ProfileMatrix, dna:Dna):Kmer = {
    def profileScore(kmer:Kmer):Float = {
      profileMatrix.zip(kmer).foldRight(1f) { case ((m, nuc), acc) =>
        acc * m(nuc)
      }
    }
    dna sliding profileMatrix.size maxBy profileScore
  }

  def score(motifs:Seq[Kmer]):Int = {
    motifs.transpose map {
      col =>
        val colSize = col.size
        colSize - (col groupBy identity map (_._2.size) max)
    } sum
  }

  def greedMotifSearch(k:Int, t:Int, dnas:List[Dna]):List[Kmer] = dnas match {
    case firstDna::restDnas =>
      firstDna sliding k map {
        motif =>
          var motifs = List(motif)
          for(i <- 0 until t - 1){
            val profileMatrix = buildProfileMatrix(motifs)
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
