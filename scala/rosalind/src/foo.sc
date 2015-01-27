

import scala.collection.mutable.ListBuffer
object foo {
//  val peptide = PeptideSeq.peptideFromString("NQEL")
//  val spectrum = "0 99 113 114 128 227 257 299 355 356 370 371 484" split "\\s+" map (_.toInt)
//  peptide foreach println
  val l = List(
    List(1, 2 ,3),
    List(2, 3, 4)
  ).to[ListBuffer]
  l -= List(1, 2 ,3)
  val ll = l.head
  for(i <- 1 to ll.size){
    println(ll(i))
  }
}