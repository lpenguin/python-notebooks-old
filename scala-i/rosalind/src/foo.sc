object foo {
	val l = List("a", "b", "c")               //> l  : List[String] = List(a, b, c)
	"a" :: l                                  //> res0: List[String] = List(a, a, b, c)
}