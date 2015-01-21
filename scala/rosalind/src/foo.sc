object foo {
  val peptide = List (1,    3, 5, 5, 7,    9)
  val peptide2 = List(1, 2,    5,       8, 9)
  val peptideMass = peptide.sum
  val prefixMasses = Array.fill(peptide.size + 1)(0)
  for((v, i) <- peptide.zipWithIndex){
    prefixMasses(i + 1) = prefixMasses(i) + v
  }
  prefixMasses

  peptide.foldLeft(List(0))((acc, v) => (acc.head+v)::acc ).reverse
  peptide.length - (peptide diff peptide2).length
  peptide2.length - (peptide2 diff peptide).length
}