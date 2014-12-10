object lia {
  import rosalind.util.IntCombinatorics._
  
  val data = "6 17"                               //> data  : String = 6 17
  val Array(k, n) = data.split("\\s+")            //> k  : String = 6
                                                  //| n  : String = 17
  
  def lia(k:Int, N:Int):Double = {
  	val children:Int = Math.pow(2, k).toInt
  	def probForK(currentK:Int):Double = {
  		Math.pow(1f/4f, currentK) * Math.pow(3f/4f, children - currentK) * (children choose currentK).toFloat
  	}
  	
  	var prob:Double = 0f
  	for(i <- N to children){
  		prob += probForK(i)
  	}
  	return prob
  }                                               //> lia: (k: Int, N: Int)Double
  
  lia(k.toInt, n.toInt)                           //> res0: Double = 0.43335211351308817
  
  /*
  **Given:** Two positive integers k (k≤7) and N (N≤2k). In this problem, we begin with Tom, who in the 0th generation has genotype Aa Bb. Tom has two children in the 1st generation, each of whom has two children, and so on. Each organism always mates with an organism having genotype Aa Bb.
	**Return:** The probability that at least N Aa Bb organisms will belong to the k-th generation of Tom's family tree (don't count the Aa Bb mates at each level). Assume that Mendel's second law holds for the factors.
	**Explanation**
	If we examine the Punnett square in Figure 1. we can see that if we combine any genotype of egg with sperm from a heterozygous individual (or vice versa), the probability of obtaining an AaBb offspring in any generation is 0.25.
	Next, the probability of there being k or more successes (offspring is AaBb) is equivalent to the probability of there being $(2^n)-k$ or less failures (offspring is not AaBb). (There are $2^n$ 'trials')
	This forms a binomial distribution. As we can see from the above, the probability of there being $k$ or more success is the binomial cumulative distribution function (cdf) with $2^n$ trials, $(2^n)-k$ "successes" and 1 - 0.25 = 0.75 probability - we treat the failures as success for the purposes of using the cdf)
	Many languages have this function built in, some will require other libraries, but generally looks like this:
	
	    cdf(2^n, (2^n)-k, 0.75)
	
	To obtain the probability of a certain outcome we can use the following mathematical formula:
	$$ y! / (x! * (y-x)!) * (0.25^x) * (0.75^y)$$
	where $x$ equals the value of the desired outcome (e.g. exactly one heterozygous offspring) and $y$ equals the total number of trials (or children in a particular generation, which is $2^n$).
	*/
}