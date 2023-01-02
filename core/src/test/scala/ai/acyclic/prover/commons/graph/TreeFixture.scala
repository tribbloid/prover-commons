package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Tree
import ai.acyclic.prover.commons.graph.viz.Hierarchy
import ai.acyclic.prover.commons.testlib.BaseSpec

abstract class TreeFixture extends BaseSpec {

  import TreeFixture._

  object Top5 extends Hierarchy.Indent2 {
    override lazy val maxDepth: Int = 5
  }

  implicit lazy val treeFormat: Hierarchy = Top5

  val tree1: TreeFixture._Tree = TN(
    "aaa",
    Seq(
      TN(
        "bbb",
        Seq(
          TN("ddd")
        )
      ),
      TN(
        "ccc"
      )
    )
  ).tree

  val tree2: TreeFixture._Tree = TN( // TODO: simplify this with graph Transform
    "aaa\n%%%%%",
    Seq(
      TN(
        "bbb\n%%%%%",
        Seq(
          TN("ddd\n%%%%%")
        )
      ),
      TN(
        "ccc\n%%%%%"
      )
    )
  ).tree

  val treeInf = TInf("abcdefgh").tree
}

object TreeFixture {

  trait Demo {

    def text: String
    def children: Seq[Demo]

    def tree: _Tree = _Tree(this)
  }

  case class TN(
      text: String,
      children: Seq[Demo] = Nil
  ) extends Demo {}

  case class TInf(
      text: String
  ) extends Demo {

    override def children: Seq[Demo] = {

      if (text.length <= 2) {
        Seq(this)
      } else {
        text.sliding(text.length - 1, 1).toSeq.map { v =>
          TInf(v)
        }
      }
    }
  }

  case class _Tree(root: Demo) extends Tree[Demo] {

    case class Ops(node: Demo) extends UpperNOps {

      override protected def getNodeText = node.text

      override protected def getInduction: Seq[Arrow.`~>`.Of[Demo]] = node.children
    }
  }
}
