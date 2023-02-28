package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.Correspondence
import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.graph.processing.GraphPlans
import ai.acyclic.prover.commons.viz.text.TextBlock
import org.scalameta.ascii
import org.scalameta.ascii.layout.GraphLayout
import org.scalameta.ascii.layout.prefs.{LayoutPrefs, LayoutPrefsImpl}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.ListSet
import scala.collection.mutable

object Hasse extends Visualisations {

  type UB[N] = Graph[N]

  trait Default extends Hasse {

    override def layoutPreferences: LayoutPrefs = LayoutPrefsImpl.DEFAULT
  }
  object Default extends Default {}

  implicit lazy val default: Default.type = Default

}

trait Hasse extends Hasse.Format {

  import Hasse._

  lazy val maxDepth = 20

  def layoutPreferences: ascii.layout.prefs.LayoutPrefs

  final lazy val _layoutPreferences = layoutPreferences

  lazy val bindings: LazyList[String] = (0 until Int.MaxValue).to(LazyList).map(v => "" + v)

  def apply[N](s: UB[N]): Viz[N] = Viz(s)

  case class Viz[N](override val graph: UB[N]) extends TextViz[N] {

    lazy val bindingIndices = new AtomicInteger(0)

    case class NodeWrapper(node: N) {

      @transient var binding: String = _
      def bindingOpt: Option[String] = Option(binding)
      def bind(): Unit = {
        binding = bindingIndices.getAndIncrement().toString
      }

      val arrowReprs: mutable.Buffer[(NodeWrapper, Option[String])] = mutable.Buffer.empty

      def bindInboundArrows(): Unit = {

        val numArrowInboundWithText = arrowReprs.count(v => v._2.nonEmpty)

        if (numArrowInboundWithText >= 2) {

          arrowReprs.foreach {
            case (v, Some(_)) => v.bind()
            case _            =>
          }
        }
      }

      lazy val nOps: graph.Ops = graph.nodeOps(node)
      final override lazy val toString = {

        val arrowBlocks = arrowReprs.flatMap {
          case (from, textOpt) =>
            textOpt.map { text =>
              val textBlock =
                TextBlock(text).encloseIn.squareBracket

              val zipped = from.bindingOpt
                .map { binding =>
                  TextBlock(s"[$binding]").zipBottom(
                    textBlock
                  )
                }
                .getOrElse {
                  textBlock
                }

              zipped
            }
        }

        val arrowTextOpt = arrowBlocks.reduceOption { (x, y) =>
          x.zipRight(y)
        }

        val nodeText = {
          val ss = bindingOpt
            .map { binding =>
              s"${nOps.nodeText} [$binding]"
            }
            .getOrElse(nOps.nodeText)
          TextBlock(ss)
        }
        val result = arrowTextOpt
          .map { arrowText =>
            arrowText.zipBottom(nodeText)
          }
          .getOrElse(nodeText)
        result.build // TODO: may have alignment problem if not converted to rectangular block
      }
    }

    lazy val asciiDiagram: org.scalameta.ascii.graph.Graph[NodeWrapper] = {

      val nodeBuffer: Correspondence[N, NodeWrapper] =
        Correspondence[N, NodeWrapper](v => NodeWrapper(v))

      val relationBuffer = mutable.Buffer.empty[(NodeWrapper, NodeWrapper)]

      val buildBuffers = GraphPlans(graph)
        .Traverse(
          maxDepth = Hasse.this.maxDepth,
          down = { node =>
            val wrapper = nodeBuffer.getOrElseUpdate(node)

            val newRelations = wrapper.nOps.induction.flatMap { arrow =>
              arrow.arrowType match {
                case Arrow.`~>` =>
                  val to = nodeBuffer.getOrElseUpdate(arrow.target)
                  to.arrowReprs += wrapper -> arrow.arrowText
                  Some(wrapper -> to)
                case Arrow.`<~` =>
                  val from = nodeBuffer.getOrElseUpdate(arrow.target)
                  wrapper.arrowReprs += from -> arrow.arrowText
                  Some(from -> wrapper)
                case _ =>
                  None
              }
            }

            relationBuffer ++= newRelations
          }
        )
        .DepthFirst_ForEach

      buildBuffers.exeOnce

      val nodeSet: Set[NodeWrapper] = nodeBuffer.values
        .map { nodeWrapper =>
          nodeWrapper.bindInboundArrows()
          nodeWrapper
        }
        .toList
        .to(ListSet)
      ascii.graph.Graph(nodeSet, relationBuffer.toList)
    }

    override def treeString: String = {

      GraphLayout.renderGraph(asciiDiagram, layoutPrefs = _layoutPreferences)
    }
  }
}
