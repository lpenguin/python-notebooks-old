package rosalind.assignments.stepic

import rosalind.assignments.stepic.common.Bioinformatics.{Kmer, Dna, fromString}
import rosalind.util.Extractor.{Int, Extract}
import rosalind.assignments.stepic.common.Motif
import scala.io.Source
import scala.util.Random

/**
 * Created by nikitaprianichnikov on 09.02.15.
 */
object ste3_6_5 {
  def randomizedMotifSearch(k:Int, t:Int, dnas:Seq[Dna]):Seq[Kmer] = {
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
      case List((motifs, _), _) => motifs
    }
  }

  def main(args: Array[String]) {
    Source.fromFile("./data/t.txt").getLines().toList match {
      case Extract(Int(k), Int(t))::dnas =>
        val res = randomizedMotifSearch(k, t, dnas map fromString)
        for(motif <- res){
          println(motif.mkString)
        }
    }
  }
}
