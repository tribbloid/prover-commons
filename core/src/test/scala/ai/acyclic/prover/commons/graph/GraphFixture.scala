package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.testlib.BaseSpec

import scala.collection.mutable.ArrayBuffer

trait GraphFixture extends BaseSpec {

  import GraphFixture._

  val diamond: OBDemo = {

    val a = new OBDemo("aaa")
    val b = new OBDemo("bbb")
    val c = new OBDemo("ccc")
    val d = new OBDemo("ddd")
    val e = new OBDemo("eee")

    a.outbound ++= Seq(b, c)
    b.outbound += d
    c.outbound += d
    d.outbound += e

    a
  }

  val cyclic: OBDemo = {

    val a = new OBDemo("aaa")
    val b = new OBDemo("bbb")
    val c = new OBDemo("ccc")
    val d = new OBDemo("ddd")

    a.outbound += b
    b.outbound ++= Seq(c, d)
    c.outbound += a

    a
  }
}

object GraphFixture {

  class OBDemo(
      val nodeText: String
  ) extends Graph.Outbound.Node {

    override lazy val outbound: ArrayBuffer[Arrow.`~>`.Of[OBDemo]] = ArrayBuffer.empty
  }
}
