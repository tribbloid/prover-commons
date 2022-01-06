package ai.acyclic.graph.commons

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
