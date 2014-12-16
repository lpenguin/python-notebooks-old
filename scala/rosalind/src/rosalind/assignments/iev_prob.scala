package rosalind.assignments

import scala.collection.mutable
import scala.util.Random

/**
 * Created by nikita on 15.12.14.
 */
object iev_prob {
  trait Specie {
    def isDominant:Boolean
  }

  object Dominant extends Specie{
    def isDominant = true
  }

  object Recessive extends Specie{
    def isDominant = false
  }

  class SpecieGenerator(prob:Float){
    def next: Specie = {
      if(Random.nextFloat() <= prob)
        Dominant
      else
        Recessive
    }
  }

  val probs =  List(1f, 1f, 1f, 0.75f, 0.5f, 0f)
  val counts = List(20,  0,  0, 400,    1000, 1)
  val generators = (probs, counts).zipped flatMap ((p, c) => List.fill(c)(new SpecieGenerator(p)))

  val mm = new mutable.HashMap[Int, Int]
  val testsCount = 200
  def main(args: Array[String]) {
    for(i <- 1 to testsCount){
      val c = generators flatMap (g => List(g.next, g.next)) count (s => s.isDominant)
      mm(c) = mm.getOrElse(c, 0) + 1
    }

    val expected = mm.foldLeft(0f)( (a, kv) => {
      a + kv._1 * ( kv._2/ testsCount.toFloat)
    })

    println(expected)
    println((probs, counts).zipped map ((p, c) => c * 2 * p) sum)
//    for(k <- mm.keys.toSeq.sorted){
//      val prob = mm(k) / (testsCount.toFloat)
//      println(s"$k: $prob")
//    }
  }
}
