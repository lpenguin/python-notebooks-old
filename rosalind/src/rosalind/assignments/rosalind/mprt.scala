package rosalind.assignments.rosalind

import rosalind.util.{FastaReader, OutputWriter}

import scala.io.Source

/**
 * Created by nikita on 11.12.14.
 */
object mprt {
  def main(args: Array[String]) {
    val uniprotUrl = "http://www.uniprot.org/uniprot/"

    def downloadFasta(name:String):FastaReader.FastaRecord = {
      FastaReader.fromUrl(uniprotUrl+name+".fasta").head
    }

    val dataSet = Source.fromFile("./data/rosalind_mprt_sample.txt").getLines()
    val motifPattern = """(?=N[^P][ST][^P])""".r

    val ow = new OutputWriter("mprt", printlnResult = true)
    for(line <- dataSet) {
      val matches = motifPattern findAllMatchIn downloadFasta(line).value
      if(matches.nonEmpty){
        ow.writeln(line)
        ow.writeln(matches.map(_.start+1).toSeq.sorted.mkString(" "))
      }
    }
    ow.close()
  }
}
