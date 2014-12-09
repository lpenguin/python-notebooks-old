object iprb{
	object Organism extends Enumeration{
	  type Organism = Value
	  val A = Value("A")
	  val B = Value("B")
	  val C = Value("C")
	}
	
	import Organism.{Organism, A, B, C}
	
	def mendel(x:Organism, y:Organism) = (x, y) match {
	  case (A, A) => 1f
	  case (A, B) => 1f
	  case (A, C) => 1f
	  case (B, B) => 0.75f
	  case (B, C) => 0.5f
	  case (_, _) => throw new Error("Bad input")
	}                                         //> mendel: (x: iprb.Organism.Organism, y: iprb.Organism.Organism)Float
	
	
	case class Population(k:Int, m:Int, n:Int){
	  val sum:Float = k + m + n
	
	  def prob(o:Organism) = o match {
	    case A => k / sum
	    case B => m / sum
	    case C => n / sum
	  }
	
	  def sub(o:Organism) = o match {
	    case A => Population(k - 1, m, n)
	    case B => Population(k, m - 1, n)
	    case C => Population(k, m, n - 1)
	  }
	
	  def prob(a:Organism, b:Organism):Float = {
	    val p = prob(a) * sub(a).prob(b) + (
	      if(a != b) prob(b)* sub(b).prob(a) else 0
	      )
	
	    mendel(a, b) * p
	  }
	}
	
	val (k, m, n) = (24, 16, 16)              //> k  : Int = 24
                                                  //| m  : Int = 16
                                                  //| n  : Int = 16
	
	
	val p = new Population(k, m, n)           //> p  : iprb.Population = Population(24,16,16)
	
	val pairs = (A, A) :: (A, B) :: (A, C) :: (B, B) :: (B, C) :: Nil
                                                  //> pairs  : List[(iprb.Organism.Value, iprb.Organism.Value)] = List((A,A), (A,
                                                  //| B), (A,C), (B,B), (B,C))
	println(pairs.map(t => p.prob(t._1, t._2)).sum)
                                                  //> 0.81948054
}