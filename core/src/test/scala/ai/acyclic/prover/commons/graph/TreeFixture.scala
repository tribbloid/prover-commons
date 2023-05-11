package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.viz.Hierarchy
import ai.acyclic.prover.commons.testlib.BaseSpec

abstract class TreeFixture extends BaseSpec {

  import TreeFixture._

  object Top5 extends Hierarchy.Indent2 {
    override lazy val maxDepth: Int = 5
  }

  implicit lazy val treeFormat: Hierarchy = Top5

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

  implicit class TVView(self: TVLike) {

    def tree =
      Local.Tree(Node(self))

    def treeWithArrowTexts =
      Local.Tree(NodeWithArrowText(self))
  }
}

object TreeFixture {

  trait TVLike {

    def text: String
    def children: Seq[TVLike]

  }

  case class TV(
      text: String,
      children: Seq[TVLike] = Nil
  ) extends TVLike {}

  case class TVInf(
      text: String
  ) extends TVLike {

    override def children: Seq[TVLike] = {

      if (text.length <= 2) {
        Seq(this)
      } else {
        text.sliding(text.length - 1, 1).toSeq.map { v =>
          TVInf(v)
        }
      }
    }
  }

  case class Node(value: TVLike) extends Local.Tree.Node[TVLike] {

    override protected def nodeTextC = value.text

    override protected def inductionC = value.children.map(v => Node(v))
  }

  case class NodeWithArrowText(value: TVLike) extends Local.Tree.Node[TVLike] {

    override protected def nodeTextC = value.text

    override protected def inductionC = {

      val children = value.children
      val result = children.map { child =>
        Arrow.`~>`.NoInfo(Some(s"${value.text} |> ${child.text}")) -> NodeWithArrowText(child)
      }
      result
    }
  }
}
