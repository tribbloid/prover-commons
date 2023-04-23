package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.Topology.TreeT
import ai.acyclic.prover.commons.graph.viz.Hierarchy
import ai.acyclic.prover.commons.testlib.BaseSpec
import local.Tree

abstract class TreeFixture extends BaseSpec {

  import TreeFixture._

  object Top5 extends Hierarchy.Indent2 {
    override lazy val maxDepth: Int = 5
  }

  implicit lazy val treeFormat: Hierarchy = Top5

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
}

object TreeFixture {

  trait TV {

    def text: String
    def children: Seq[TV]

//    def tree: _Tree = _Tree(this)
//
//    def treeWithArrowTexts: _TreeWithArrowTexts = _TreeWithArrowTexts(this)

    def tree = Tree(Node(this))

    def treeWithArrowTexts = Tree(Node(this))
  }

  case class TVF(
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

  case class Node(value: TV) extends TreeT.Node[TV] {

    override protected def getNodeText = value.text

    override protected def getInduction = value.children.map(v => Node(v))
  }

  case class NodeWithArrowText(value: TV) extends TreeT.Node[TV] {

    override protected def getNodeText = value.text

    override protected def getInduction = {

      val children = value.children
      val result = children.map { child =>
        Arrow.`~>`.NoInfo(Some(s"${value.text} |> ${child.text}")) -> Node(child)
      }
      result
    }
  }
}
