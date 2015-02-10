package rosalind.assignments.stepic.common

import rosalind.assignments.stepic.common.Bioinformatics._
import rosalind.util.Prelude._

import scala.collection.immutable.Stream.Empty
/**
 * Created by nikita on 29.01.15.
 */
object Motif {
  type ProfileMatrix = Seq[Map[Nucleotide.Value, Float]]

  def buildProfileMatrixLaplass(motifs:Seq[Kmer]):ProfileMatrix =
    motifs.transpose map { col =>
      val colSize = (col.size + 4).toFloat
      (col groupBy identity map {
        x => (x._1,  (x._2.size + 1) / colSize)
      }).toMap.withDefaultValue(1f/colSize)
    }

  def kmerProfileScore(profileMatrix:ProfileMatrix)(kmer:Kmer):Float = {
    profileMatrix.zip(kmer).foldRight(1f) { case ((probs, nuc), acc) =>
      acc * probs(nuc)
    }
  }

  def profileMostProbableKmer(profileMatrix: ProfileMatrix, dna:Dna):Kmer = {
    dna sliding profileMatrix.size maxBy kmerProfileScore(profileMatrix)
  }

  def score(motifs:Seq[Kmer]):Int = {
    motifs.transpose map {
      col =>
        val colSize = col.size
        colSize - (col groupBy identity map (_._2.size) max)
    } sum
  }

  val dictStream = (dict map (_::Nil)).toStream
  def enumeratePatterns(n:Int):Stream[CharList] = n match {
    case 1 => dictStream
    case _ =>
      val subPatterns = enumeratePatterns(n - 1)
      subPatterns flatMap (p => dict map (_::p))
  }
}
