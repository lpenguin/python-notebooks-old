package rosalind.assignments.stepic.chapter3

import rosalind.assignments.common.Bioinformatics.{Dna, Kmer, fromString}
import rosalind.assignments.common.Motif._
import rosalind.util.Extractor.{Extract, Int}
import rosalind.util.Prelude
import rosalind.util.Prelude.Task

import scala.util.Random

/**
 * Created by nikitaprianichnikov on 10.02.15.
 */
object ste3_9_3 {
  def main(args: Array[String]) {
    GibbsSampling.run(Prelude.fromData("dataset_163_4")) foreach println
  }

  object GibbsSampling extends Task{
    override def run(data: List[String]): Seq[String] = {
      data match {
        case Extract(Int(k), Int(t), Int(n)) :: dnas =>
          1 to 20 map { x =>
            println(s"Iteration #$x")
            val motifs = gibbsSampling(dnas map fromString, k, t, n)
            val sc = score(motifs)
            println(s"Got motifs with score: $sc")
            (sc, motifs)
          } minBy {
            case (score, _) => score
          } match {
            case (_, motifs) => motifs map (_.mkString)
          }
      }
    }

    def gibbsSampling(dnas:Seq[Dna], k:Int, t:Int, n:Int):Seq[Kmer] = {
      def buildProbabilityDistribution(dna:Dna, profileMatrix: ProfileMatrix):Iterable[Float] = {
        dna sliding k map kmerProfileScore(profileMatrix) toIterable
      }

      def genRandom(probs:Iterable[Float]):Int = {
        def takeAtSum(v:Float) = {
          probs.foldLeft((0, 0f)::Nil){
            case (p@(index, sum)::xs, item) => (index + 1, sum + item)::p
          } find {
            case (index, sum) => sum <= v
          } match {
            case Some((index, _)) => index
          }
        }

        takeAtSum(Random.nextFloat() * probs.sum)
      }

      def newMotifs(motifs:List[Kmer]):List[Kmer] = {
        val i = Random.nextInt(t)
        val (left, _::right) = motifs splitAt i
        val profileMatrix = buildProfileMatrixLaplass(left ++ right)
        val dna = dnas(i)
        val probDistribution = buildProbabilityDistribution(dna, profileMatrix)
        val randPosition = genRandom(probDistribution)
        val newMotif = dna.slice(randPosition, randPosition + k)

        left ++ (newMotif :: right)
      }

      val randomMotifs = dnas map { dna =>
        val n = Random.nextInt(dna.size - k)
        dna.slice(n, n + k)
      } toList

      Iterator.iterate((0, randomMotifs, score(randomMotifs))){
        case (index, motifs, motifsScore) =>
          val ms = newMotifs(motifs)
          (index + 1, ms, score(ms))
      } takeWhile {
        case (index, _, _) => index <= n
      } minBy {
        case (_, _, motifsScore) => motifsScore
      } match {
        case (_, motifs, _) => motifs
      }
    }
  }
}
