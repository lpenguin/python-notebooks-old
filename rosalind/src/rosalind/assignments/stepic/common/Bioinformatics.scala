package rosalind.assignments.stepic.common



/**
 * Created by nikitaprianichnikov on 09.02.15.
 */
object Bioinformatics{

  object Nucleotide extends Enumeration{
    val Nucleotide = Value
    val A, C, T, G, U = Value


  }

  type Nucleotide = Nucleotide.Value
  import Nucleotide._

  def fromChar(c:Char):Nucleotide = c match {
    case 'A' => A
    case 'C' => C
    case 'G' => G
    case 'T' => T
    case 'U' => U
    case _ => throw new Error(s"Invalid nucleotide: $c")
  }

  type Nucleotides = Seq[Nucleotide.Value]
  type Dna = Nucleotides
  type Kmer = Nucleotides

  def fromString(string: String):Nucleotides =
    string.toStream map fromChar
}
