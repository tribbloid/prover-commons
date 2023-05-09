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

  val tn1 = TVProduct(
    "aaa",
    Seq(
      TVProduct(
        "bbb",
        Seq(
          TVProduct("ddd")
        )
      ),
      TVProduct(
        "ccc"
      )
    )
  )

  val tn2 = TVProduct( // TODO: simplify this with graph Transform
    "aaa\n%%%%%",
    Seq(
      TVProduct(
        "bbb\n%%%%%",
        Seq(
          TVProduct("ddd\n%%%%%")
        )
      ),
      TVProduct(
        "ccc\n%%%%%"
      )
    )
  )

  val treeInf = TVInf("abcdefgh")
}

object TreeFixture {

  trait TV {

    def text: String
    def children: Seq[TV]

    def tree =
      Local.Tree(Node(this))

    def treeWithArrowTexts =
      Local.Tree(NodeWithArrowText(this))
  }

  case class TVProduct(
      text: String,
      children: Seq[TV] = Nil
  ) extends TV {}

  case class TVInf(
      text: String
  ) extends TV {

    override def children: Seq[TV] = {

      if (text.length <= 2) {
        Seq(this)
      } else {
        text.sliding(text.length - 1, 1).toSeq.map { v =>
          TVInf(v)
        }
      }
    }
  }

  case class Node(value: TV) extends Local.Tree.Node[TV] {

    override protected def nodeTextC = value.text

    override protected def inductionC = value.children.map(v => Node(v))
  }

  case class NodeWithArrowText(value: TV) extends Local.Tree.Node[TV] {

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
