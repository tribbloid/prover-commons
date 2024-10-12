package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.local.Local.AnyGraph

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

  object GV extends Local.AnyGraph.Outbound.Inspection[GV] {

    case class Node(override val value: GV) extends OGraphNode {

      override protected def getInduction =
        value.children.toSeq.map(v => Node(v))
    }

    object WithArrows extends Local.AnyGraph.Outbound.Inspection[GV] {

      case class Node(override val value: GV) extends OGraphNode {

        override protected def getInduction = {
          val children = value.children.toSeq
          val result = children.map { child =>
            Arrow.Outbound.NoInfo(Some(s"${value.text} |> ${child.text}")) -> Node(child)
          }
          result
        }
      }
    }

    implicit class GVOps(vs: Seq[GV]) {

      def withArrows: WithArrows.ValuesOps = WithArrows.ValuesOps(vs: Seq[GV])

    }
  }

  trait OGraphNode extends Local.AnyGraph.Outbound.NodeImpl[GV] {

    override protected def getNodeText = value.text

    override def evalCacheKeyC: Option[GV] = Some(value)

    override def identityKeyC: Option[Any] = Some(value.text)
  }

  case class GVRewriter(builder: GV => Local.AnyGraph.Node[GV]) extends Local.AnyGraph.RewriterImpl[GV] {

    override def rewrite(src: AnyGraph.Node[GV])(
        inductions: Seq[AnyGraph.Node[GV]]
    ): Local.AnyGraph.Node[GV] = {

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

  object Appendage {

    val p1: Seq[GV] = {

      val b = GV("bbb")
      val d = GV("eee")

      b.children += d
      Seq(b)
    }
  }

}
