package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.local.Local.Graph

import java.util.UUID
import scala.collection.mutable.ArrayBuffer

object GraphFixture {

  case class GV(
      text: String,
      id: UUID = UUID.randomUUID()
  )(
      initialChildren: Seq[GV] = Nil
  ) {

    val children: ArrayBuffer[GV] = ArrayBuffer(initialChildren: _*)
  }

  case class Node(override val value: GV) extends Local.Graph.Outbound.Node[GV] {

    override protected def nodeTextC = value.text

    override protected def inductionC =
      value.children.toSeq.map(v => Node(v))

    override lazy val evalCacheKey: Option[GV] = Some(value)

    override lazy val identityKey: Option[Any] = Some(value.text)
  }

  //  {
  //    // sanity
  //
  //    implicitly[Node <:< Local.Graph.NodeCompat[GV]]
  //    implicitly[Node <:< Local.Graph.Outbound.NodeCompat[GV]]
  //  }

  case class NodeWithArrowText(override val value: GV) extends Local.Graph.Outbound.Node[GV] {

    override protected def nodeTextC = value.text

    override protected def inductionC = {
      val children = value.children.toSeq
      val result = children.map { child =>
        Arrow.`~>`.NoInfo(Some(s"${value.text} |> ${child.text}")) -> NodeWithArrowText(child)
      }
      result
    }

    override lazy val evalCacheKey: Option[GV] = Some(value)

    override lazy val identityKey: Option[Any] = Some(value.text)
  }

  case class GVRewriter(builder: GV => Local.Graph.NodeCompat[GV]) extends Local.Graph.Rewriter[GV] {

    override def rewrite(src: Graph.NodeCompat[GV])(
        inductions: Seq[Graph.NodeCompat[GV]]
    ): Local.Graph.NodeCompat[GV] = {

      val result = src.value.copy()(Nil)
      result.children.clear()
      result.children.addAll(inductions.map(_.value))
      builder(result)
    }
  }

  // instances:

  val diamond: Seq[GV] = {

    val d = GV("ddd")(
      Seq(GV("eee")())
    )

    Seq(
      GV("aaa")(
        Seq(
          GV("bbb")(Seq(d)),
          GV("ccc")(Seq(d))
        )
      )
    )
  }

  val cyclic: Seq[GV] = {

    val a = GV("aaa")()
    val b = GV("bbb")()
    val c = GV("ccc")()
    val d = GV("ddd")()

    a.children += b
    b.children ++= Seq(c, d)
    c.children += a

    Seq(a)
  }

  val cyclic2: Seq[GV] = {

    val a = GV("aaa\n%%%%")()
    val b = GV("bbb\n%%%%")()
    val c = GV("ccc\n%%%%")()
    val d = GV("ddd\n%%%%")()

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
