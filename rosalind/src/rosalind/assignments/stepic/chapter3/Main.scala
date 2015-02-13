package rosalind.assignments.stepic.chapter3

import rosalind.assignments.stepic.chapter3.ste3_4_9.MedianString.medianString
import rosalind.assignments.stepic.chapter3.ste3_6_5.RandomizedMotifSearch.randomizedMotifSearch
import rosalind.assignments.common.{Bioinformatics, Motif}
import rosalind.util.Extractor.Int
import rosalind.util.Prelude.{Task, fromData}

/**
 * Created by nikitaprianichnikov on 12.02.15.
 */
object Main {
  def runTask(task:Task, dataName:String): Unit ={
    for(res <- task.run(fromData(dataName))){
      println(res)
    }
  }

  def main(args: Array[String]) {
    val dnas = fromData("DosR")
    val res = args.toList match {
      case name::Int(k)::xs =>
        name match {
          case "median" =>
            println("Median search")
            medianString(k, dnas)::Nil

          case "random" =>
            println("Randomized search")
            xs match {
              case Int(count)::xss =>
                val res = 1 to count map { i =>
//                  print(s"#$i ")
                  randomizedMotifSearch(k, dnas.size, dnas map Bioinformatics.fromString)
                } minBy {
                  case (_, score) => score
                }

                res match {
                  case (motifs, score) =>
                    val out = motifs map (motif => s"${motif.mkString}") toList;
                    val cons = Motif.consensusString(motifs)
                    "Consensus: "::s"${cons.mkString} $score"::"Motifs: "::out
                }
            }

        }

    }
    res foreach println
  }
}
