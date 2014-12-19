package rosalind.assignments

import rosalind.util.FastaReader

/**
 * Created by nikita on 18.12.14.
 */

import rosalind.util.CodonTable

object orf {
  /*
  http://rosalind.info/problems/orf/
  */
  def main(args: Array[String]) {


    def findGenes(s:Stream[Char]):List[String] = s match {
      case 'M'#::xs =>
        val (gene, rest) = cropGene(s)
        if(rest.isEmpty){
          Nil
        }else{
          gene.mkString :: findGenes(xs)
        }
      case x#::xs => findGenes(xs)
      case Stream() => Nil

    }

    def cropGene(s:Stream[Char]) = {
      s span (_ != '_')
    }

    def translateGene(s:Stream[Char]) = {
      s sliding (3, 3) filter (_.size == 3) map (triplet => CodonTable.dna(triplet.mkString)) toStream
    }

    val dnaString = FastaReader.fromData("rosalind_orf_sample").head.value

    val s = dnaString.toStream
    val rs = s.reverse.map{
      case 'T' => 'A'
      case 'A' => 'T'
      case 'G' => 'C'
      case 'C' => 'G'
    }

    val ss = (0 to 2) flatMap (n => List(s drop n, rs drop n))

    val set = new scala.collection.mutable.LinkedHashSet[String]

    for(seq <- ss){
      findGenes(translateGene(seq)) foreach {q =>
        if(set.add(q)){
          println(q)
        }
      }
    }

//    set foreach println
  }
}
