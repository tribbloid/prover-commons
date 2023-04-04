package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.graph.plan.local.GraphUnary
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

      val arrowsFrom: mutable.Buffer[(NodeWrapper, Option[String])] = mutable.Buffer.empty

      def bindInboundArrows(): Unit = {

        val numArrowInboundWithText = arrowsFrom.count(v => v._2.nonEmpty)

        if (numArrowInboundWithText >= 2) {

          arrowsFrom.foreach {
            case (v, Some(_)) => v.bind()
            case _            =>
          }
        }
      }

      lazy val nOps = graph.ops(node)
      final override lazy val toString = {

        val showBinding = arrowsFrom.size >= 2

        val arrowBlocks = arrowsFrom
          .flatMap {
            case (from, textOpt) =>
              textOpt.map { text =>
                val textBlock =
                  TextBlock(text)

                val zipped = from.bindingOpt
                  .filter(_ => showBinding)
                  .map { binding =>
                    TextBlock(s"from [$binding]").zipBottom(
                      textBlock
                    )
                  }
                  .getOrElse {
                    textBlock
                  }

                zipped.encloseIn.parenthesis
              }
          }

        val arrowTextOpt = arrowBlocks
          .reduceOption { (x, y) =>
            x.zipRight(y)
          }
          .map(_.rectangular)

        val nodeText = {
          val ss = bindingOpt
            .map { binding =>
              s"${nOps.nodeText}\n[$binding]"
            }
            .getOrElse(nOps.nodeText)
          TextBlock(ss).rectangular
        }
        val result = arrowTextOpt
          .map { arrowText =>
            arrowText.pad
              .bottom('─')
              .zipBottom(nodeText)
          }
          .getOrElse(nodeText)
        result.build
      }
    }

    lazy val asciiDiagram: org.scalameta.ascii.graph.Graph[NodeWrapper] = {

      val nodeBuffer = graph.sameness.Memoize[N, NodeWrapper](v => NodeWrapper(v))

      val relationBuffer = mutable.Buffer.empty[(NodeWrapper, NodeWrapper)]

      val buildBuffers = GraphUnary
        .make(graph)
        .Traverse(
          maxDepth = Hasse.this.maxDepth,
          down = { node =>
            val wrapper = nodeBuffer.apply(node)

            val newRelations = wrapper.nOps.discoverArrow.flatMap { v =>
              v._1.arrowType match {
                case Arrow.`~>` =>
                  val to = nodeBuffer.apply(v._2)
                  to.arrowsFrom += wrapper -> v._1.arrowText
                  Some(wrapper -> to)
                case Arrow.`<~` =>
                  val from = nodeBuffer.apply(v._2)
                  wrapper.arrowsFrom += from -> v._1.arrowText
                  Some(from -> wrapper)
                case _ =>
                  None
              }
            }

            relationBuffer ++= newRelations
          }
        )
        .DepthFirst_Once

      buildBuffers.exeOnce

      val nodeSet: Set[NodeWrapper] = nodeBuffer.outer.values
        .map { nodeWrapper =>
          nodeWrapper.bindInboundArrows()
          nodeWrapper
        }
        .toList
        .to(ListSet)
      ascii.graph.Graph(nodeSet, relationBuffer.toList)
    }

    override lazy val treeString: String = {

      GraphLayout.renderGraph(asciiDiagram, layoutPrefs = _layoutPreferences)
    }
  }
}
