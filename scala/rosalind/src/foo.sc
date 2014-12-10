object foo {
	import rosalind.ListCombinatorics._
	import rosalind.IntCombinatorics._
	val l = List("a", "b", "c", "d", "e", "f")//> l  : List[String] = List(a, b, c, d, e, f)
	"a" :: l                                  //> res0: List[String] = List(a, a, b, c, d, e, f)
	
	l.dropRight(1)                            //> res1: List[String] = List(a, b, c, d, e)
	combinations(l, 4).size                   //> res2: Int = 15
	l.size choose 4                           //> res3: scala.math.BigDecimal = 15
	l choose 2                                //> res4: List[List[String]] = List(List(a, b), List(a, c), List(a, d), List(a, 
                                                  //| e), List(a, f), List(b, c), List(b, d), List(b, e), List(b, f), List(c, d), 
                                                  //| List(c, e), List(c, f), List(d, e), List(d, f), List(e, f))
}