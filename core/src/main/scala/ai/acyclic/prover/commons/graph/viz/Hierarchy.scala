package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.local.Semilattice
import ai.acyclic.prover.commons.viz.text.{Padding, TextBlock}

object Hierarchy extends Visualisations {

  type UB[N] = Semilattice.Upper[N]

  implicit lazy val default: Hierarchy.Indent2.type = Hierarchy.Indent2

  trait Indent2 extends Hierarchy
  case object Indent2 extends Indent2

  trait Indent2Minimal extends Hierarchy {

    override lazy val FORK: Padding = Padding("", "")
    override lazy val LEAF: Padding = Padding("", "")
    override lazy val SUB: Padding = Padding(" ‣ ", " : ")

    override lazy val DOT = ""
  }
  case object Indent2Minimal extends Indent2Minimal
}

trait Hierarchy extends Hierarchy.Format {

  import Hierarchy._

  lazy val maxDepth: Int = 20

  lazy val FORK: Padding = Padding("-+", " :")
  lazy val LEAF: Padding = Padding("--", "  ")

  lazy val SUB: Padding = Padding(" !", " :")
  lazy val SUB_LAST: Padding = SUB.copy(body = SUB.body.map(_ => ' '))

  lazy val DOT = " "

  def apply[N](s: UB[N]): Viz[N] = Viz(s)

  case class Viz[N](override val graph: UB[N]) extends TextViz[N] {

    case class SubViz(head: N, depth: Int = maxDepth) {

      lazy val headOps: graph.UpperNOps = graph.nodeOps(head)

      lazy val treeString: String = {

        val wText = TextBlock(headOps.nodeText).indent(DOT)

        if (headOps.isLeaf || depth <= 0) {

          wText.padLeft(LEAF).build

        } else {

          val selfT = wText.padLeft(FORK)

          val children = headOps.children

          val childrenTProtos: Seq[TextBlock] = children.map { child =>
            val childViz = this.copy(child, depth - 1)
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
