package rosalind.util


object CodonTable {
  private val rnaCodonTableStr:String = """UUU F      CUU L      AUU I      GUU V
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

  private val dnaCodonTableStr:String = """TTT F      CTT L      ATT I      GTT V
TTC F      CTC L      ATC I      GTC V
TTA L      CTA L      ATA I      GTA V
TTG L      CTG L      ATG M      GTG V
TCT S      CCT P      ACT T      GCT A
TCC S      CCC P      ACC T      GCC A
TCA S      CCA P      ACA T      GCA A
TCG S      CCG P      ACG T      GCG A
TAT Y      CAT H      AAT N      GAT D
TAC Y      CAC H      AAC N      GAC D
TAA Stop   CAA Q      AAA K      GAA E
TAG Stop   CAG Q      AAG K      GAG E
TGT C      CGT R      AGT S      GGT G
TGC C      CGC R      AGC S      GGC G
TGA Stop   CGA R      AGA R      GGA G
TGG W      CGG R      AGG R      GGG G"""


  def codonStringToTable(s:String) = {
    val aminoAcidsCountsMut = new scala.collection.mutable.HashMap[Char, Int]()

    s.split("\\s+")
      .sliding(2, 2)
      .map((t:Array[String]) => {
      val Array(nb, aa) = t
      nb -> (aa match {
        case "Stop" => stopChar
        case x => x.head
      })
    }).toMap
  }

  val stopChar = '_'
  val startAminoAcid = 'M'
  val rna = codonStringToTable(rnaCodonTableStr)
  val dna = codonStringToTable(dnaCodonTableStr)

  def translateGene(map:Map[String, Char])(s:Stream[Char]) = {
    s sliding (3, 3) filter (_.size == 3) map (triplet => map(triplet.mkString)) toStream
  }

  def translateGeneDna(s:Stream[Char]) = translateGene(dna)(s)

  def translateGeneRna(s:Stream[Char]) = translateGene(rna)(s)

}