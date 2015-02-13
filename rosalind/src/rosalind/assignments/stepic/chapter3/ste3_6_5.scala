package rosalind.assignments.stepic.chapter3

import rosalind.assignments.common.Bioinformatics.{Dna, Kmer, fromString}
import rosalind.assignments.common.Motif
import rosalind.util.Extractor.{Extract, Int}
import rosalind.util.Prelude
import rosalind.util.Prelude.Task

import scala.util.Random

/**
 * Created by nikitaprianichnikov on 09.02.15.
 */
object ste3_6_5 {
  def main(args: Array[String]) {
    RandomizedMotifSearch.run(Prelude.fromData("t"))
  }

  object RandomizedMotifSearch extends Task{
    override def run(data: List[String]): Seq[String] = {
      data match {
        case Extract(Int(k), Int(t))::dnas =>
          randomizedMotifSearch(k, t, dnas map fromString) match {
            case (motifs, score) =>
              motifs map (motif => s"${motif.mkString} $score")
          }
      }
    }

    def randomizedMotifSearch(k:Int, t:Int, dnas:Seq[Dna]):(Seq[Kmer], Int) = {
      val randomMotifs = dnas map { dna =>
        val n = Random.nextInt(dna.size - k)
        dna.slice(n, n + k)
      }
      val firstScore = Motif.score(randomMotifs)
      val bestMotifTIt = Iterator.iterate(randomMotifs -> firstScore){
        case (motifs, score) =>
          val profileMatrix = Motif.buildProfileMatrixLaplass(motifs)
          val newMotifs = dnas map (Motif.profileMostProbableKmer(profileMatrix, _))
          val newScore = Motif.score(newMotifs)
          newMotifs -> newScore
      } sliding 2 dropWhile {case List(x, y) => x._2 > y._2}

      bestMotifTIt.next() match {
        case List((motifs, score), _) => motifs -> score
      }
    }
  }
}
