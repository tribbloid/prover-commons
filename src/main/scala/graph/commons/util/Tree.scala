package graph.commons.util

trait TreeNode {

  def nodeStr: String

  lazy val children: Seq[TreeNode] = Nil

}

object Tree {

  val FORK = " :-+ "
  val LEAF = " :-  "
  val WRAP = " |   "

}
