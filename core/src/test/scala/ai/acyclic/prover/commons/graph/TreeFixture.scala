package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.viz.Hierarchy
import ai.acyclic.prover.commons.testlib.BaseSpec

abstract class TreeFixture extends BaseSpec {

  implicit lazy val treeFormat: Hierarchy = Hierarchy.Indent2

  import TreeFixture.TDemo

  val tree1 = TDemo(
    "aaa",
    Seq(
      TDemo(
        "bbb",
        Seq(
          TDemo("ddd")
        )
      ),
      TDemo(
        "ccc"
      )
    )
  )

  val tree2 = TDemo( // TODO: simplify this with graph TransformPlan
    "aaa\n%%%%%",
    Seq(
      TDemo(
        "bbb\n%%%%%",
        Seq(
          TDemo("ddd\n%%%%%")
        )
      ),
      TDemo(
        "ccc\n%%%%%"
      )
    )
  )
}

object TreeFixture {

  case class TDemo(
      nodeText: String,
      override val outbound: Seq[Arrow.`~>`.Of[TDemo]] = Nil
  ) extends Tree.Node {}
}
