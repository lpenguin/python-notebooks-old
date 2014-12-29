package rosalind.util

import rosalind.graphs.{TreeNode, Tree}

/**
 * Created by nikita on 29.12.14.
 */
object TreePrinter {
  def print(tree:Tree) = {
    printNode(tree.root, isLast = true) foreach println
  }

  private def printNode(node:TreeNode, isLast:Boolean):List[String] = {
    val indent = if(isLast) "   " else "|  "
    val name= s"|->$node"

    def iter(childs:List[TreeNode]):List[String] = childs match {
      case Nil => Nil
      case x::Nil => printNode(x, isLast = true) map (indent+_)
      case x::xs => (printNode(x, isLast = false) map (indent+_)) ::: iter(xs)
    }

    name :: iter(node.leafs)
  }
}
