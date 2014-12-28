object foo {

  val s1 = "a"
  val s2 = "abq"
  val (eq, noteq) = s1 zipAll (s2, '-', '-') span ( t => t._1 == t._2)
  eq map (_._1)
  val (a, b) = noteq unzip
  def prefixDiff(s1:String, s2:String):(String, String, String) = {
    val marker = '-'
    val (eq, noteq) = s1.toStream zipAll (s2.toStream, marker, marker) span (t => t._1 == t._2)
    val (d1, d2) = noteq.unzip
    (eq map (_._1) mkString,
      d1 takeWhile ( _ != '-' ) mkString, d2 takeWhile ( _ != marker ) mkString)
  }

  prefixDiff(s1, s2)

  import scala.collection.immutable.Stream.Empty
  def iter(s:Stream[Char]):List[String] = s match {
    case Empty => Nil
    case x#::xs => s.mkString :: iter(xs)
  }
  val x = Set(1, 2, 3)
  val y = Set(2, 1, 31)
  x == y

  "123".last.toString.toInt
}