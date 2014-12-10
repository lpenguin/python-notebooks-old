import java.io.FileWriter

object grph {
	import rosalind.ListCombinatorics._
	import rosalind.IntCombinatorics._
	import scala.collection.immutable.StringOps._
	import rosalind.StringUtils._
	import rosalind.FastaReader._
	
	case class Node(name:String, value:String)
	case class Edge(from:Node, to:Node)
	
	val nodes = readFasta("/home/nikita/sources/ipnb/python-notebooks/scala/rosalind/rosalind_grph.txt")
                                                  //> nodes  : List[rosalind.FastaReader.FastaRecord] = List(rosalind.FastaReader$
                                                  //| FastaRecord@16f1468c, rosalind.FastaReader$FastaRecord@3a4e9d18, rosalind.Fa
                                                  //| staReader$FastaRecord@7f2bb08f, rosalind.FastaReader$FastaRecord@d54bd83, ro
                                                  //| salind.FastaReader$FastaRecord@3665ea4b, rosalind.FastaReader$FastaRecord@5c
                                                  //| f8fdd3, rosalind.FastaReader$FastaRecord@5ee05754, rosalind.FastaReader$Fast
                                                  //| aRecord@5a857c75, rosalind.FastaReader$FastaRecord@71ae13c0, rosalind.FastaR
                                                  //| eader$FastaRecord@5b92dc8e, rosalind.FastaReader$FastaRecord@6ce1d9a, rosali
                                                  //| nd.FastaReader$FastaRecord@45f96cf3, rosalind.FastaReader$FastaRecord@7a5fe9
                                                  //| 68, rosalind.FastaReader$FastaRecord@2ed4e99c, rosalind.FastaReader$FastaRec
                                                  //| ord@1c3518c9, rosalind.FastaReader$FastaRecord@61ee4296, rosalind.FastaReade
                                                  //| r$FastaRecord@6155b814, rosalind.FastaReader$FastaRecord@42a85af8, rosalind.
                                                  //| FastaReader$FastaRecord@3aec71f8, rosalind.FastaReader$FastaRecord@792e6f00,
                                                  //|  rosalind.FastaReader$Fa
                                                  //| Output exceeds cutoff limit.

                 	                                 
  val k = 3                                       //> k  : Int = 3
  val fw = new FileWriter("/home/nikita/sources/ipnb/python-notebooks/scala/rosalind/graph_out.txt", false)
                                                  //> fw  : java.io.FileWriter = java.io.FileWriter@30a4fe7c
  
  try{
	  for(List(n1, n2) <- nodes choose 2){
	  	if(n1.value.overlaps(n2.value, k))
	  		fw.write(n1.name+" "+n2.name+"\n")
	  }
  }finally fw.close()
}