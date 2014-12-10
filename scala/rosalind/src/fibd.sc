object fibd {
	// http://rosalind.info/problems/fibd/
	def rab(n:Int, k:Int) = {
		val precalc = new scala.collection.mutable.HashMap[(Int, Int), Long]
		
		def raba(age:Int, n:Int):Long = {
			
			precalc.get((age, n)) match {
				case Some(x) => x
				case None => {
					val res = age match {
						case 1 => if(n == 1) 1 else (2 to k).map{raba(_, n - 1)}.sum
						case i => if(n == 1) 0 else raba(i - 1, n - 1)
					}
					precalc((age, n)) = res
					res
				}
			}
		}
		(1 to k).map{raba(_, n)}.sum

	}                                         //> rab: (n: Int, k: Int)Long
	
	
	// Solution from rosalind (python)
	def rabCycle(n:Int, k:Int):Long = {
		var ages = List(1l) ++ List.fill(k - 1)(0l)
		
		for(i <- 1 to n - 1){
			ages = List(ages.drop(1).sum) ++ ages.dropRight(1)
		}
		
		ages.sum
	}                                         //> rabCycle: (n: Int, k: Int)Long
	
	val data = "81 16"                        //> data  : String = 81 16
	val Array(n, k) = data.split("\\s")       //> n  : String = 81
                                                  //| k  : String = 16
	rab(n.toInt, k.toInt)                     //> res0: Long = 37378265960028808
	rabCycle(n.toInt, k.toInt)                //> res1: Long = 37378265960028808
	
}