package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons
import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.topology.Induction
import ai.acyclic.prover.commons.graph.viz.Hierarchy

object TreeFixture {

  trait TV {

    def text: String
    def children: collection.Seq[TV]
  }

  case class TVF(
      text: String,
      children: Seq[TVF] = Nil
  ) extends TV {}

  case class TVInf(
      text: String
  ) extends TV {

    override def children: Seq[TVInf] = {

      if (text.length <= 2) {
        Seq(this)
      } else {
        text.sliding(text.length - 1, 1).toSeq.map { v =>
          TVInf(v)
        }
      }
    }
  }

  trait TreeNode extends Local.Tree.Node_[TV] {

    final override protected def getNodeText: String = value.text
  }

  case class node(value: TV) extends TreeNode {

    override protected def getInduction: Seq[(Arrow.`~>`, node)] =
      value.children.map(v => node(v)).toSeq
  }

  case class NodeWithArrowText(value: TV) extends TreeNode {

    override protected def getInduction: Seq[(commons.graph.Arrow.Outbound, NodeWithArrowText)] = {

      val children = value.children
      val result = children.map { child =>
        Arrow.Outbound.OfText(Some(s"${value.text} |> ${child.text}")) -> NodeWithArrowText(child)
      }
      result.toSeq
    }
  }

  // instances:

  implicit lazy val treeFormat: Hierarchy = {

    object Top5 extends Hierarchy.Indent2 {
      override lazy val maxDepth: Int = 5
    }
    Top5
  }

  val tn1: TVF = TVF(
    "aaa",
    Seq(
      TVF(
        "bbb",
        Seq(
          TVF("ddd")
        )
      ),
      TVF(
        "ccc"
      )
    )
  )

  val tn2: TVF = TVF( // TODO: simplify this with graph Transform
    "aaa\n%%%%%",
    Seq(
      TVF(
        "bbb\n%%%%%",
        Seq(
          TVF("ddd\n%%%%%")
        )
      ),
      TVF(
        "ccc\n%%%%%"
      )
    )
  )

  val treeInf: TVInf = TVInf("abcdefgh")

  implicit class TVView(self: TV) {

    def tree: LocalEngine._GraphK.Aux[Induction.TreeT, TV] =
      Local.Tree(node(self))

    def treeWithArrowTexts: LocalEngine._GraphK.Aux[Induction.TreeT, TV] =
      Local.Tree(NodeWithArrowText(self))
  }
}
