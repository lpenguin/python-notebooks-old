package rosalind.assignments.rosalind

/**
 * Created by nikita on 29.12.14.
 * rosalind.info/problems/perm/
 */
object perm {
  def main(args: Array[String]) {
    val count = 6
    val perms =  (1 to count).permutations.toList
    println(perms.size)
    perms foreach (x => println(x.mkString(" ")))
  }
}
