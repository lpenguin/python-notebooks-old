case class A(data:Int, score:Int)

val data = List(
  new A(1, 1),
  new A(2, 1),
  new A(3, 1),
  new A(4, 2),
  new A(5, 2),
  new A(6, 3),
  new A(7, 4)
)

(data groupBy (_.score) toList) sortBy (_._1) take 3