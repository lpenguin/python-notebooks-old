object grph {
	import rosalind.ListCombinatorics._
	import rosalind.IntCombinatorics._
	
	case class Node(name:String, value:String)
	case class Edge(from:Node, to:Node)
	
	val data = """>Rosalind_0498
AAATAAA
>Rosalind_2391
AAATTTT
>Rosalind_2323
TTTTCCC
>Rosalind_0442
AAATCCC
>Rosalind_5013
GGGTGGG"""                                        //> data  : String = >Rosalind_0498
                                                  //| AAATAAA
                                                  //| >Rosalind_2391
                                                  //| AAATTTT
                                                  //| >Rosalind_2323
                                                  //| TTTTCCC
                                                  //| >Rosalind_0442
                                                  //| AAATCCC
                                                  //| >Rosalind_5013
                                                  //| GGGTGGG


	val nodes = data.split("\\s+").sliding(2, 2).map(x => {
		val Array(name, value) = x
		Node(name.drop(1), value)
	}).toList                                 //> nodes  : List[grph.Node] = List(Node(Rosalind_0498,AAATAAA), Node(Rosalind_2
                                                  //| 391,AAATTTT), Node(Rosalind_2323,TTTTCCC), Node(Rosalind_0442,AAATCCC), Node
                                                  //| (Rosalind_5013,GGGTGGG))

	nodes choose 2                            //> res0: List[List[grph.Node]] = List(List(Node(Rosalind_0498,AAATAAA), Node(Ro
                                                  //| salind_2391,AAATTTT)), List(Node(Rosalind_0498,AAATAAA), Node(Rosalind_2323,
                                                  //| TTTTCCC)), List(Node(Rosalind_0498,AAATAAA), Node(Rosalind_0442,AAATCCC)), L
                                                  //| ist(Node(Rosalind_0498,AAATAAA), Node(Rosalind_5013,GGGTGGG)), List(Node(Ros
                                                  //| alind_2391,AAATTTT), Node(Rosalind_2323,TTTTCCC)), List(Node(Rosalind_2391,A
                                                  //| AATTTT), Node(Rosalind_0442,AAATCCC)), List(Node(Rosalind_2391,AAATTTT), Nod
                                                  //| e(Rosalind_5013,GGGTGGG)), List(Node(Rosalind_2323,TTTTCCC), Node(Rosalind_0
                                                  //| 442,AAATCCC)), List(Node(Rosalind_2323,TTTTCCC), Node(Rosalind_5013,GGGTGGG)
                                                  //| ), List(Node(Rosalind_0442,AAATCCC), Node(Rosalind_5013,GGGTGGG)))

}