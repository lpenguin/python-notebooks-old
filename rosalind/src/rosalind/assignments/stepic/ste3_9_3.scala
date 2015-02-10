package rosalind.assignments.stepic

import rosalind.assignments.stepic.common.Bioinformatics.{Kmer, Dna, fromString}
import rosalind.util.Extractor.{Int, Extract}
import rosalind.assignments.stepic.common.Motif._
import scala.io.Source
import scala.util.Random

/**
 * Created by nikitaprianichnikov on 10.02.15.
 */
object ste3_9_3 {
  def gibbsSampling(dnas:Seq[Dna], k:Int, t:Int, n:Int):Seq[Kmer] = {
    def buildProbabilityDistribution(dna:Dna, profileMatrix: ProfileMatrix):Iterable[Float] = {
      dna sliding k map kmerProfileScore(profileMatrix) toIterable
    }

    def genRandom(probs:Iterable[Float]):Int = {
      def takeAtSum(probs:Iterable[Float], v:Float) = {
        probs.foldLeft((0, 0f)::Nil){
          case (p@(index, sum)::xs, item) => (index + 1, sum + item)::p
        } find {
          case (index, sum) => sum <= v
        } match {
          case Some((index, _)) => index
        }
      }

      takeAtSum(probs, Random.nextFloat() * probs.sum)
    }

    def newMotifs(motifs:List[Kmer]):List[Kmer] = {
      val i = Random.nextInt(t)
      val (left, _::right) = motifs splitAt i
      val profileMatrix = buildProfileMatrixLaplass(left ++ right)
      val dna = dnas(i)
      val probDistribution = buildProbabilityDistribution(dna, profileMatrix)
      val randPosition = genRandom(probDistribution)
      val newMotif:Kmer = dna.slice(randPosition, randPosition + k)

      left ++ (newMotif :: right)
    }

    val randomMotifs = dnas map { dna =>
      val n = Random.nextInt(dna.size - k)
      dna.slice(n, n + k)
    } toList;
    Iterator.iterate((0, randomMotifs, score(randomMotifs))){
      case (index, motifs, motifsScore) =>
        val ms = newMotifs(motifs)
        (index + 1, ms, score(ms))
    } takeWhile {
      case (index, _, _) => index <= n
    } maxBy {
      case (_, _, motifsScore) => motifsScore
    } match {
      case (_, motifs, _) => motifs
    }
  }

  def main(args: Array[String]) {
    Source.fromFile("./data/t.txt").getLines().toList match {
      case Extract(Int(k), Int(t), Int(n))::dnas =>
        val res = 1 to 100 map { x =>
          println(s"Iteration #$x")
          val motifs = gibbsSampling(dnas map fromString, k, t, n)
          val sc = score(motifs)
          println(s"Got motifs with score: $sc")
          (sc, motifs)
        } minBy {
          case (score, _) => score
        } match {
          case (_, motifs) => motifs
        }

        for(motif <- res){
          println(motif.mkString)
        }

    }
  }
}
