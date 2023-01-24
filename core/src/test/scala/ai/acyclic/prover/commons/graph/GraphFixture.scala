package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.testlib.BaseSpec

import scala.collection.mutable.ArrayBuffer

trait GraphFixture extends BaseSpec {

  import GraphFixture._

  val diamond: _Outbound = {

    val a = GDemo("aaa")
    val b = GDemo("bbb")
    val c = GDemo("ccc")
    val d = GDemo("ddd")
    val e = GDemo("eee")

    a.children ++= Seq(b, c)
    b.children += d
    c.children += d
    d.children += e

    a
  }.graph

  val cyclic: _Outbound = {

    val a = GDemo("aaa")
    val b = GDemo("bbb")
    val c = GDemo("ccc")
    val d = GDemo("ddd")

    a.children += b
    b.children ++= Seq(c, d)
    c.children += a

    a
  }.graph

  val cyclic2: _Outbound = {

    val a = GDemo("aaa\n%%%%")
    val b = GDemo("bbb\n%%%%")
    val c = GDemo("ccc\n%%%%")
    val d = GDemo("ddd\n%%%%")

    a.children += b
    b.children ++= Seq(c, d)
    c.children += a

    a
  }.graph
}

object GraphFixture {

  case class GDemo(
      text: String
  ) {

    lazy val children: ArrayBuffer[GDemo] = ArrayBuffer.empty

    def graph: _Outbound = _Outbound(Seq(this))
  }

  case class _Outbound(override val roots: Seq[GDemo]) extends Graph.Outbound[GDemo] {

    case class Ops(node: GDemo) extends OutboundNOps {

      override protected def getNodeText = node.text

      override protected def getInduction: Seq[Arrow.`~>`.Of[GDemo]] = node.children.toSeq
    }
  }
}
