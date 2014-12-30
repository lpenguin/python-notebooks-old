package rosalind.assignments.rosalind

import rosalind.graphs.{SuffixTree, Tree, TreeNode}
import rosalind.util.FastaReader
import rosalind.util.Profiler._

/**
 * Created by nikita on 28.12.14.
 * http://rosalind.info/problems/lcsm/
 */

/*
Simple python solution:
def substr_in_all(arr, part):
  for dna in arr:
    if dna.find(part)==-1:
      return False
  return True

def common_substr(arr, l):
  first = arr[0]
  for i in range(len(first)-l+1):
    part = first[i:i+l]
    if substr_in_all(arr, part):
      return part
  return ""

def longest_common_substr(arr):
  l = 0; r = len(arr[0])

  while l+1<r:
    mid = (l+r) / 2
    if common_substr(arr, mid)!="":
      l=mid
    else:
      r=mid

  return common_substr(arr, l)
 */
object lcsm2 {
  def main(args: Array[String]) {
    val data = FastaReader.fromData("rosalind_lcsm")

    val tree = new Tree(TreeNode("/".toStream)) with SuffixTree
    tree longestCommonString data.map(_.value) foreach ( x => println(x))
    traceMem()

//    TreePrinter.print(tree)
  }
}
