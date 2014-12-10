package rosalind.util

object CodonTable {
  private val codonTableStr:String = """UUU F      CUU L      AUU I      GUU V
  UUC F      CUC L      AUC I      GUC V
  UUA L      CUA L      AUA I      GUA V
  UUG L      CUG L      AUG M      GUG V
  UCU S      CCU P      ACU T      GCU A
  UCC S      CCC P      ACC T      GCC A
  UCA S      CCA P      ACA T      GCA A
  UCG S      CCG P      ACG T      GCG A
  UAU Y      CAU H      AAU N      GAU D
  UAC Y      CAC H      AAC N      GAC D
  UAA Stop   CAA Q      AAA K      GAA E
  UAG Stop   CAG Q      AAG K      GAG E
  UGU C      CGU R      AGU S      GGU G
  UGC C      CGC R      AGC S      GGC G
  UGA Stop   CGA R      AGA R      GGA G
  UGG W      CGG R      AGG R      GGG G"""
  
  val stopChar = '_'
  private val aminoAcidsCounsMut = new scala.collection.mutable.HashMap[Char, Int]()

  val table = codonTableStr.split("\\s+")
  .sliding(2, 2)
  .map((t:Array[String]) => {
    val Array(nb, aa) = t
    nb -> (aa match {
      case "Stop" => stopChar
      case x => x.head
    })
  }).toMap
  
  for((nb, aa) <- table){
    val counts = aminoAcidsCounsMut.getOrElse(aa, 0)
    aminoAcidsCounsMut(aa) = counts + 1
  }
  
  val aminoAcidsCounts = aminoAcidsCounsMut.toMap
  
 
}