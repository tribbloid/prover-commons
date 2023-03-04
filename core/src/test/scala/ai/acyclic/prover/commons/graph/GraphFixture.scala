package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.testlib.BaseSpec

import scala.collection.mutable.ArrayBuffer

trait GraphFixture extends BaseSpec {

  import GraphFixture._

  val diamond: GN = {

    val a = GN("aaa")
    val b = GN("bbb")
    val c = GN("ccc")
    val d = GN("ddd")
    val e = GN("eee")

    a.children ++= Seq(b, c)
    b.children += d
    c.children += d
    d.children += e

    a
  }

  val cyclic: GN = {

    val a = GN("aaa")
    val b = GN("bbb")
    val c = GN("ccc")
    val d = GN("ddd")

    a.children += b
    b.children ++= Seq(c, d)
    c.children += a

    a
  }

  val cyclic2: GN = {

    val a = GN("aaa\n%%%%")
    val b = GN("bbb\n%%%%")
    val c = GN("ccc\n%%%%")
    val d = GN("ddd\n%%%%")

    a.children += b
    b.children ++= Seq(c, d)
    c.children += a

    a
  }
}

object GraphFixture {

  case class GN(
      text: String
  ) {

    lazy val children: ArrayBuffer[GN] = ArrayBuffer.empty

    def graph: _OGraph = _OGraph(Seq(this))

    def graphWithArrowText: _OGraphWithArrowText = _OGraphWithArrowText(Seq(this))
  }

  case class _OGraph(override val roots: Seq[GN]) extends Graph.Outbound[GN] {

    case class Ops(node: GN) extends OutboundNOps {

      override protected def getNodeText = node.text

      override protected def getInduction: Seq[Arrow.`~>`.Of[GN]] = node.children.toSeq
    }
  }

  case class _OGraphWithArrowText(override val roots: Seq[GN]) extends Graph.Outbound[GN] {

    case class Ops(node: GN) extends OutboundNOps {

      override protected def getNodeText = node.text

      override protected def getInduction: Seq[Arrow.`~>`.Of[GN]] = {

        val children = node.children.toSeq
        val result = children.map { child =>
          Arrow.`~>`.NoInfo(child, Some(s"${node.text} |> ${child.text}"))
        }
        result
      }
    }
  }
}