package org.shapesafe.graph.commons.util

case class TreeFormatFixtures(
    format: TreeFormat
) {

  case class Demo(
      nodeString: String,
      override val children: Seq[Demo] = Nil
  ) extends TreeLike {

    final override lazy val treeFormat: TreeFormat = format
  }
}
