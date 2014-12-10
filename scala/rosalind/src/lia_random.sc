object lia {
	import rosalind.IntCombinatorics._
	
	object Allel extends Enumeration {
		type Allel = Value
		val A = Value("A")
		val a = Value("a")
		val B = Value("B")
		val b = Value("b")

	}
	
	
	import Allel._
	import scala.util.Random
	
	class Gene(val a1:Allel, val a2:Allel){
		def eq(other:Gene) =
			(a1 == other.a1 && a2 == other.a2) ||
			(a2 == other.a1 && a1 == other.a2)
		
		
		def randChoose: Allel = {
			if(Random.nextInt % 2 == 0) a1
			else a2
		}
		
		override def toString = a1+""+a2
	}
	
	object Gene{
		def apply(a1:Allel, a2:Allel) = new Gene(a1, a2)
	}
	
	class Cat(val geneA:Gene, val geneB:Gene) {
	 	override def toString = "["+geneA+" "+geneB+"]"
		def eq(other:Cat) = geneA.eq(other.geneA) && geneB.eq(other.geneB)
			
		def merge(other:Cat):Cat = {
			val (a1, b1) = (geneA.randChoose, geneB.randChoose)
			val (a2, b2) = (other.geneA.randChoose, other.geneB.randChoose)
			return new Cat(Gene(a1, a2), Gene(b1, b2))
		}
	}
	
	val mammy = new Cat(Gene(A, a), Gene(B, b))
                                                  //> mammy  : lia.Cat = [Aa Bb]
	val tom = new Cat(Gene(A, a), Gene(B, b)) //> tom  : lia.Cat = [Aa Bb]
	
	def maxK = 2                              //> maxK: => Int

	def catMerge(cat: Cat, k:Int = 0):List[Cat] = {
		if(k == maxK)
			List(cat)
		else {
			val lefts = catMerge(cat.merge(mammy), k + 1)
			val rights = catMerge(cat.merge(mammy), k + 1)
			lefts ++ rights
		}
	}                                         //> catMerge: (cat: lia.Cat, k: Int)List[lia.Cat]

	def countCats(cats: List[Cat], otherCat:Cat):Int = {
		var c = 0
		for(cat <- cats){
			if(otherCat.eq(cat)){
				c += 1
			}
		}
		return c
	}                                         //> countCats: (cats: List[lia.Cat], otherCat: lia.Cat)Int
	
	val maxC = 20                             //> maxC  : Int = 20
	var good = 0                              //> good  : Int = 0
	for(i <- 1 to maxC){
		if(countCats(catMerge(tom), tom) == 1){
			good += 1
		}
	}
	Math.pow(1f/4f, 2)                        //> res0: Double = 0.0625
	(3f *3f*3f)/(4f*4f*4f)                    //> res1: Float = 0.421875
	good.toFloat / maxC                       //> res2: Float = 0.55
	

}