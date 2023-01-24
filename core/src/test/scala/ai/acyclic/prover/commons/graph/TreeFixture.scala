package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Tree
import ai.acyclic.prover.commons.graph.viz.Hierarchy
import ai.acyclic.prover.commons.testlib.BaseSpec

abstract class TreeFixture extends BaseSpec {

  import TreeFixture._

  implicit lazy val treeFormat: Hierarchy = Hierarchy.Indent2

  val tree1: TreeFixture._Tree = TDemo(
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
  ).tree

  val tree2: TreeFixture._Tree = TDemo( // TODO: simplify this with graph Transform
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
  ).tree
}

object TreeFixture {

  case class TDemo(
      text: String,
      children: Seq[TDemo] = Nil
  ) {

    def tree: _Tree = _Tree(this)
  }

  case class _Tree(root: TDemo) extends Tree[TDemo] {

    case class Ops(node: TDemo) extends UpperNOps {

      override protected def getNodeText = node.text

      override protected def getInduction: Seq[Arrow.`~>`.Of[TDemo]] = node.children
    }
  }
}
