package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.viz.Hierarchy

object TreeFixture {

  trait _TV {

    def text: String
    def children: collection.Seq[_TV]
  }

  case class TV(
      text: String,
      children: Seq[TV] = Nil
  ) extends _TV {}

  case class TVInf(
      text: String
  ) extends _TV {

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

  case class Node(value: _TV) extends Local.Tree.Node[_TV] {

    override protected def nodeTextC = value.text

    override protected def inductionC = value.children.map(v => Node(v)).toSeq
  }

  case class NodeWithArrowText(value: _TV) extends Local.Tree.Node[_TV] {

    override protected def nodeTextC = value.text

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

  val tn1 = TV(
    "aaa",
    Seq(
      TV(
        "bbb",
        Seq(
          TV("ddd")
        )
      ),
      TV(
        "ccc"
      )
    )
  )

  val tn2 = TV( // TODO: simplify this with graph Transform
    "aaa\n%%%%%",
    Seq(
      TV(
        "bbb\n%%%%%",
        Seq(
          TV("ddd\n%%%%%")
        )
      ),
      TV(
        "ccc\n%%%%%"
      )
    )
  )

  val treeInf = TVInf("abcdefgh")

  implicit class TVView(self: _TV) {

    def tree =
      Local.Tree(Node(self))

    def treeWithArrowTexts =
      Local.Tree(NodeWithArrowText(self))
  }
}
