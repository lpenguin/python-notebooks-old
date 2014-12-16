package rosalind.assignments

/**
 * Created by nikita on 16.12.14.
 */
object cons {
  def main(args: Array[String]) {
    val ll = List(
      "CCTG".toCharArray,
      "AGTG".toCharArray,
      "ACGA".toCharArray
    )

    ll.transpose.map(l => {
      val lens = l.groupBy(l => l).map((t) => (t._1, t._2.length) )

      (
        lens.getOrElse('A', 0),
        lens.getOrElse('C', 0),
        lens.getOrElse('T', 0),
        lens.getOrElse('G', 0)
        )
    })
  }
}
