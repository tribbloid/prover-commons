package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.local.Semilattice
import ai.acyclic.prover.commons.viz.text.{Padding, TextBlock}

object Hierarchy extends Visualisations {

  type UB[N] = Semilattice.Upper[N]

  implicit lazy val default: Hierarchy.Indent2.type = Hierarchy.Indent2

  trait BuiltIn extends Hierarchy {}

  case object Indent2 extends BuiltIn

  case object Indent2Minimal extends BuiltIn {

    override lazy val FORK: Padding = Padding("", "")
    override lazy val LEAF: Padding = Padding("", "")
    override lazy val SUB: Padding = Padding(" ‣ ", " : ")

    override lazy val DOT = ""
  }
}

trait Hierarchy extends Hierarchy.Format {

  import Hierarchy._

  lazy val FORK: Padding = Padding("-+", " :")
  lazy val LEAF: Padding = Padding("--", "  ")

  lazy val SUB: Padding = Padding(" !", " :")
  lazy val SUB_LAST: Padding = SUB.copy(body = SUB.body.map(_ => ' '))

  lazy val DOT = " "

  def apply[N](s: UB[N]): Viz[N] = Viz(s)

  case class Viz[N](override val graph: UB[N]) extends TextViz[N] {

    case class SubViz(first: N) {

      lazy val firstOps: graph.UpperNOps = graph.nodeOps(first)

      lazy val treeString: String = {

        val wText = TextBlock(firstOps.nodeText).indent(DOT)

        if (firstOps.isLeaf) {

          wText.padLeft(LEAF).build

        } else {

          val selfT = wText.padLeft(FORK)

          val children = firstOps.children

          val childrenTProtos: Seq[TextBlock] = children.map { child =>
            val childViz = this.copy(child)
            TextBlock(childViz.treeString)
          }

          val childrenTMid = childrenTProtos.dropRight(1).map { tt =>
            tt.padLeft(SUB)
          }

          val childrenTLast = childrenTProtos.lastOption.map { tt =>
            tt.padLeft(SUB_LAST)
          }.toSeq

          val result = (Seq(selfT) ++ childrenTMid ++ childrenTLast)
            .map { v =>
              v.build
            }
            .mkString("\n")

          result
        }
      }
    }

    override def treeString: String = SubViz(graph.root).treeString
  }

}
