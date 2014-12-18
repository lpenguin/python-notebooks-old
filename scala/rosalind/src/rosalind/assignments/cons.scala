package rosalind.assignments

/**
 * Created by nikita on 16.12.14.
 */
object cons {
  /*
  http://rosalind.info/problems/cons/
  */
  import rosalind.util.FastaReader._
  import rosalind.util.OutputWriter
  def getCurrentDirectory = new java.io.File( "." ).getCanonicalPath

  def main(args: Array[String]) {
    val ll = fromData("rosalind_cons") map ( r => r.value.toCharArray)

    val counts = ll.transpose.map(l => {
      l.groupBy(l => l).map((t) => (t._1, t._2.length) )
    })


    val ow = new OutputWriter("rosalind_cons", true)
    ow writeln (counts map (c => c.maxBy((t) => t._2)._1) mkString "")
    for(nuc <- List('A', 'C', 'G', 'T')){
      ow writeln (nuc+": "+(counts map (c => c.getOrElse(nuc, 0)) mkString " "))
    }
    ow.close()
  }

}
