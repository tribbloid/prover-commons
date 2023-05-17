package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.local.Local.Graph

import java.util.UUID
import scala.collection.mutable.ArrayBuffer

object GraphFixture {

  case class GV(
      text: String,
      initialChildren: Seq[GV] = Nil,
      id: UUID = UUID.randomUUID()
  ) {

    val children: ArrayBuffer[GV] = ArrayBuffer(initialChildren: _*)
  }

  trait OGraphNode extends Local.Graph.Outbound.NodeImpl[GV] {

    override protected def nodeTextC = value.text

    override lazy val evalCacheKey: Option[GV] = Some(value)

    override lazy val identityKey: Option[Any] = Some(value.text)
  }

  case class Node(override val value: GV) extends OGraphNode {

    override protected def inductionC =
      value.children.toSeq.map(v => Node(v))
  }

  case class NodeWithArrowText(override val value: GV) extends OGraphNode {

    override protected def inductionC = {
      val children = value.children.toSeq
      val result = children.map { child =>
        Arrow.`~>`.NoInfo(Some(s"${value.text} |> ${child.text}")) -> NodeWithArrowText(child)
      }
      result
    }
  }

  case class GVRewriter(builder: GV => Local.Graph.Node[GV]) extends Local.Graph.RewriterImpl[GV] {

    override def rewrite(src: Graph.Node[GV])(
        inductions: Seq[Graph.Node[GV]]
    ): Local.Graph.Node[GV] = {

      val result = src.value.copy()
      result.children.clear()
      result.children.addAll(inductions.map(_.value))
      builder(result)
    }
  }

  // instances:

  val diamond: Seq[GV] = {

    val d = GV(
      "ddd",
      Seq(GV("eee"))
    )

    Seq(
      GV(
        "aaa",
        Seq(
          GV("bbb", Seq(d)),
          GV("ccc", Seq(d))
        )
      )
    )

  }

  val cyclic: Seq[GV] = {

    val a = GV("aaa")
    val b = GV("bbb")
    val c = GV("ccc")
    val d = GV("ddd")

    a.children += b
    b.children ++= Seq(c, d)
    c.children += a

    Seq(a)
  }

  val cyclic2: Seq[GV] = {

    val a = GV("aaa\n%%%%")
    val b = GV("bbb\n%%%%")
    val c = GV("ccc\n%%%%")
    val d = GV("ddd\n%%%%")

    a.children += b
    b.children ++= Seq(c, d)
    c.children += a

    Seq(a)
  }

  //  object DuplicatedB {
  //
  //    val part1: Seq[GV] = {
  //      val a = GV("aaa")
  //      val b = GV("bbb")
  //      val c = GV("ccc")
  //
  //      a.children += b
  //      b.children += c
  //      c.children += b
  //
  //      Seq(a)
  //    }
  //
  //    val part2: Seq[GV] = {
  //
  //      val b2 = GV("bbb")
  //      val d = GV("ddd")
  //
  //      b2.children += d
  //      Seq(b2)
  //    }
  //
  //    val full: Seq[GV] = {
  //
  //      val a = GV("aaa")
  //      val b = GV("bbb")
  //      val c = GV("ccc")
  //      val d = GV("ddd")
  //      val b2 = GV("bbb")
  //
  //      a.children += b
  //      b.children += c
  //      c.children += b2
  //      b2.children += d
  //
  //      Seq(a)
  //    }
  //  }

  implicit class GVsView(self: Seq[GV]) {

    def graph =
      Graph.Outbound(self.map(v => Node(v)): _*)

    def graphWithArrowText =
      Graph.Outbound(self.map(v => NodeWithArrowText(v)): _*)
  }
}
