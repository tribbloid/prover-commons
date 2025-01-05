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

    val children: ArrayBuffer[GV] = ArrayBuffer(initialChildren*)
  }

  object GV extends Local.Diverging.Graph.Inspection[GV] {

    implicitly[OGraphNode <:< Local.Diverging.Graph.Node_[GV]]

//    override val node = { (value: GV) => new node(value) }

    object inspect extends (GV => inspect)
    case class inspect(
        override val value: GV
    ) extends OGraphNode {

      override lazy val inductions: Seq[(Arrow.`~>`, inspect)] =
        value.children.toSeq.map(v => inspect(v))
    }

    object InspectGV extends Local.Diverging.Graph.Inspection[GV] {

      object inspect extends (GV => inspect)
      case class inspect(override val value: GV) extends OGraphNode {

        override lazy val inductions: Seq[
          (Arrow.`~>`, inspect)
        ] = {
          val children = value.children.toSeq
          val result = children.map { child =>
            Arrow.Outbound.OfText(Some(s"${value.text} |> ${child.text}")) -> inspect(child)
          }
          result
        }
      }
    }

    implicit class GVOps(vs: Seq[GV]) {

      def withArrows = vs.map(InspectGV.inspect)

    }
  }

  trait OGraphNode extends Local.Diverging.Graph.Node_[GV] {

    override lazy val nodeText = value.text

    override def evalCacheKeyC: Option[GV] = Some(value)

    override def identityC: Option[Any] = Some(value.text)
  }

  case class GVSetter(builder: GV => Local.AnyGraph.Node[GV]) extends Local.AnyGraph.Setter_[GV] {

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
