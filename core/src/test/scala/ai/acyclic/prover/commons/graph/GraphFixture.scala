package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.testlib.BaseSpec

import scala.collection.mutable.ArrayBuffer

trait GraphFixture extends BaseSpec {

  import GraphFixture._

  val diamond: _Graph = {

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
  }.graph

  val cyclic: _Graph = {

    val a = GN("aaa")
    val b = GN("bbb")
    val c = GN("ccc")
    val d = GN("ddd")

    a.children += b
    b.children ++= Seq(c, d)
    c.children += a

    a
  }.graph

  val cyclic2: _Graph = {

    val a = GN("aaa\n%%%%")
    val b = GN("bbb\n%%%%")
    val c = GN("ccc\n%%%%")
    val d = GN("ddd\n%%%%")

    a.children += b
    b.children ++= Seq(c, d)
    c.children += a

    a
  }.graph
}

object GraphFixture {

  case class GN(
      text: String
  ) {

    lazy val children: ArrayBuffer[GN] = ArrayBuffer.empty

    def graph: _Graph = _Graph(Seq(this))
  }

  case class _Graph(override val roots: Seq[GN]) extends Graph.Outbound[GN] {

    case class Ops(node: GN) extends OutboundNOps {

      override protected def getNodeText = node.text

      override protected def getInduction: Seq[Arrow.`~>`.Of[GN]] = node.children.toSeq
    }
  }
}
