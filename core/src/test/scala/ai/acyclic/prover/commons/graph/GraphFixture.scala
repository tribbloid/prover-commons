package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.Topology.GraphT
import ai.acyclic.prover.commons.testlib.BaseSpec

import local.Local.Graph

import scala.collection.mutable.ArrayBuffer

trait GraphFixture extends BaseSpec {

  import GraphFixture._

  val diamond: GV = {

    val a = GV("aaa")
    val b = GV("bbb")
    val c = GV("ccc")
    val d = GV("ddd")
    val e = GV("eee")

    a.children ++= Seq(b, c)
    b.children += d
    c.children += d
    d.children += e

    a
  }

  val cyclic: GV = {

    val a = GV("aaa")
    val b = GV("bbb")
    val c = GV("ccc")
    val d = GV("ddd")

    a.children += b
    b.children ++= Seq(c, d)
    c.children += a

    a
  }

  val cyclic2: GV = {

    val a = GV("aaa\n%%%%")
    val b = GV("bbb\n%%%%")
    val c = GV("ccc\n%%%%")
    val d = GV("ddd\n%%%%")

    a.children += b
    b.children ++= Seq(c, d)
    c.children += a

    a
  }
}

object GraphFixture {

  case class GV(
      text: String
  ) {

    lazy val children: ArrayBuffer[GV] = ArrayBuffer.empty

    def graph =
      Graph(Node(this))

    def graphWithArrowText =
      Graph(NodeWithArrowText(this))
  }

  case class Node(override val value: GV) extends GraphT.OutboundT.NodeEx[GV] {

    override protected def nodeTextC = value.text

    override protected def inductionC =
      value.children.toSeq.map(v => Node(v))
  }

  case class NodeWithArrowText(override val value: GV) extends GraphT.OutboundT.NodeEx[GV] {

    override protected def nodeTextC = value.text

    override protected def inductionC = {
      val children = value.children.toSeq
      val result = children.map { child =>
        Arrow.`~>`.NoInfo(Some(s"${value.text} |> ${child.text}")) -> NodeWithArrowText(child)
      }
      result
    }
  }

  case class GVRewriter(builder: GV => GraphT.NodeEx[GV]) extends GraphT.RewriterCompat[GV] {

    override def rewrite(src: GraphT.NodeCompat[GV])(
        inductions: Seq[GraphT.NodeCompat[GV]]
    ): GraphT.NodeEx[GV] = {

      val result = src.value.copy()
      result.children.addAll(inductions.map(_.value))
      builder(result)
    }
  }
}
