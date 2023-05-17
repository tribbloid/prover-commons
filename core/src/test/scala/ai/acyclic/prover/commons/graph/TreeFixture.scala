package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Local
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

  trait TreeNode extends Local.Tree.NodeImpl[TV] {

    final override protected def nodeTextC = value.text
  }

  case class Node(value: TV) extends TreeNode {

    override protected def inductionC = value.children.map(v => Node(v)).toSeq
  }

  case class NodeWithArrowText(value: TV) extends TreeNode {

    override protected def inductionC = {

      val children = value.children
      val result = children.map { child =>
        Arrow.`~>`.NoInfo(Some(s"${value.text} |> ${child.text}")) -> NodeWithArrowText(child)
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

  val tn1 = TVF(
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

  val tn2 = TVF( // TODO: simplify this with graph Transform
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

  val treeInf = TVInf("abcdefgh")

  implicit class TVView(self: TV) {

    def tree =
      Local.Tree(Node(self))

    def treeWithArrowTexts =
      Local.Tree(NodeWithArrowText(self))
  }
}
