package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.local.Semilattice
import ai.acyclic.prover.commons.viz.text.{Padding, TextBlock}

object Hierarchy extends Visualisations {

  type UB[N] = Semilattice.Upper[N]

  implicit lazy val default: Hierarchy.Indent2.type = Hierarchy.Indent2

  trait Indent2 extends Hierarchy
  case object Indent2 extends Indent2

  trait Indent2Minimal extends Hierarchy {

    override lazy val FORK: Padding = Padding.ofHead("", "")
    override lazy val LEAF: Padding = Padding.ofHead("", "")
    override lazy val SUB: Padding = Padding.ofHead(" ‣ ", " : ")

    override lazy val SPACE = ""
  }
  case object Indent2Minimal extends Indent2Minimal
}

trait Hierarchy extends Hierarchy.Format {

  import Hierarchy._

  lazy val maxDepth: Int = 20

  lazy val FORK: Padding = Padding.ofHead("+", ":")
  lazy val LEAF: Padding = Padding.ofHead("-", " ")

  lazy val SUB: Padding = Padding.ofHead("!", ":")
  lazy val SUB_LAST: Padding = SUB.keepHead(
    SUB.body.map(_ => ' ')
  )

  lazy val ARROW: Padding = Padding.ofHead(": ", ": ")

  lazy val SPACE = " "

  def apply[N](s: UB[N]): Viz[N] = Viz(s)

  case class Viz[N](override val graph: UB[N]) extends TextViz[N] {

    case class SubViz(head: N, depth: Int = maxDepth) {

      lazy val headOps: graph.UpperNOps = graph.nodeOps(head)

      lazy val treeString: String = {

        val wText = TextBlock(headOps.nodeText).indent(SPACE)

        if (headOps.isLeaf || depth <= 0) {

          wText.pad.left(LEAF).build

        } else {

          val selfT = wText.pad.left(FORK)

          val arrows = headOps.induction
          val target2Arrow = arrows
            .groupBy { v =>
              v.target
            }

          val children = arrows.map(_.target).distinct

          val childrenTProtos: Seq[TextBlock] = children.toList.map { child =>
            val _arrows = target2Arrow(child)
            val arrowBlocksOpt = _arrows
              .flatMap { arrow =>
                arrow.arrowText.map { text =>
                  TextBlock(text).encloseIn.squareBracket.pad.left(ARROW)
                }
              }
              .reduceOption((x, y) => x.zipBottom(y))

            val childViz = this.copy(child, depth - 1)
            val childBlock = TextBlock(childViz.treeString)

            val all: TextBlock = arrowBlocksOpt.map(v => v.zipBottom(childBlock)).getOrElse(childBlock)
            all.pad.left(LEAF)
          }

          val childrenTMid = childrenTProtos.dropRight(1).map { tt =>
            tt.pad.left(SUB)
          }

          val childrenTLast = childrenTProtos.lastOption.map { tt =>
            tt.pad.left(SUB_LAST)
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
