package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Arrow.Outbound
import ai.acyclic.prover.commons.graph.Engine
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.typesetting.{Padding, TextBlock}

object Hierarchy {

  trait Indent2 extends Hierarchy
  case object Indent2 extends Indent2
  implicit def Default: Indent2.type = Indent2

  trait Indent2Minimal extends Hierarchy {

    override lazy val FORK: Padding = Padding.ofHead("", "")
    override lazy val LEAF: Padding = Padding.ofHead("", "")
    override lazy val SUB: Padding = Padding.ofHead(" ‣ ", " : ")

    override lazy val SPACE = ""
  }
  case object Indent2Minimal extends Indent2Minimal {}
}

trait Hierarchy extends Visualisation.OfType with Engine.HasMaxRecursionDepth {

  override val applicableToType: Local.Semilattice.Upper.type = Local.Semilattice.Upper

  override lazy val maxDepth: Int = 10

  lazy val FORK: Padding = Padding.ofHead("+", ":")
  lazy val LEAF: Padding = Padding.ofHead("-", " ")

  lazy val SUB: Padding = Padding.ofHead("!", ":")
  lazy val SUB_LAST: Padding = SUB.keepHead(
    SUB.body.map(_ => ' ')
  )

  lazy val ARROW: Padding = Padding.ofHead(": ", ": ")

  lazy val SPACE = " "

  final override def visualise[V](data: Local.Semilattice.Upper[V]): Viz[V] = {

    Viz(data)
  }

  case class Viz[V](override val unbox: Graph_/\[V]) extends Visualized[V] {

    case class SubViz(head: Local.Semilattice.Upper.Node[V], depth: Int = maxDepth) {

      lazy val treeString: String = {

        val wText = TextBlock(head.nodeText).indent(SPACE)

        if (head.isLeaf || depth <= 0) {

          wText.pad.left(LEAF).build

        } else {

          val selfT = wText.pad.left(FORK)

          val arrows_targets: Seq[(Outbound, Local.Semilattice.Upper.Node[V])] =
            head.induction

          // TODO: if mutliple arrows in a induction are all pointing to the same target
          //  they will be displayed separately which is verbose
          //  they can be aggregated but it can cause uncessary reference binding for LinkedHierarchy
          //  as a result, no aggregation is done for now

          val childrenTProtos: Seq[TextBlock] = arrows_targets.toList.map {
            case (arrow, target) =>
              val arrows = Seq(arrow)

              val arrowBlocksOpt = if (arrows.size == 1 && arrows.forall(_.arrowText.isEmpty)) {

                None
              } else {

                arrows
                  .map { arrow =>
                    val text = arrow.arrowText.getOrElse("")

                    TextBlock(text) // .encloseIn.squareBracket
                  }
                  .reduceOption((x, y) => x.zipBottom(y))
                  .map { block =>
                    block.encloseIn.squareBracket
                  }
              }

              val childViz: SubViz = SubViz(target, depth - 1)
              val childBlock = TextBlock(childViz.treeString)

              val all: TextBlock = arrowBlocksOpt.map(v => v.zipRight(childBlock)).getOrElse(childBlock)
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

    lazy val treeText: String = {
      unbox.maxNodeOpt
        .map { nn =>
          SubViz(nn).treeString
        }
        .mkString("\n")
    }

    override def toString: String = treeText
  }
}
