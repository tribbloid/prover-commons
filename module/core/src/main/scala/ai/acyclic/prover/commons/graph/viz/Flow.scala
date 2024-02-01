package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.function.PreDef
import ai.acyclic.prover.commons.graph.{Arrow, Engine}
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.local.ops.AnyGraphUnary
import ai.acyclic.prover.commons.same.Same
import ai.acyclic.prover.commons.typesetting.TextBlock
import org.scalameta.ascii
import org.scalameta.ascii.layout.GraphLayout
import org.scalameta.ascii.layout.prefs.{LayoutPrefs, LayoutPrefsImpl}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.ListSet
import scala.collection.mutable

object Flow extends Visualisations {

  type Graph_/\[V] = Local.AnyGraph[V]

  trait Default extends Flow {

    override def layoutPreferences: LayoutPrefs = LayoutPrefsImpl.DEFAULT
  }
  object Default extends Default {}

  implicit lazy val default: Default.type = Default

}

trait Flow extends Flow.Format with Engine.HasMaxRecursionDepth {

  import Flow._
  import Local.AnyGraph._

  override lazy val maxDepth: Int = 20

  def layoutPreferences: ascii.layout.prefs.LayoutPrefs

  final lazy val _layoutPreferences = layoutPreferences

  lazy val bindings: LazyList[String] = (0 until Int.MaxValue).to(LazyList).map(v => "" + v)

  def apply[V](s: Graph_/\[V]): Viz[V] = Viz(s)

  val sameness = Same.ByEquality.Of[Node[_]](v => v.identityKey)

  case class Viz[V](override val semilattice: Graph_/\[V]) extends TextViz[V] {

    lazy val bindingIndices = new AtomicInteger(0)

    case class NodeWrapper(override val samenessDelegatedTo: Node[V]) extends sameness.Aspect {

      @transient var binding: String = _
      def bindingOpt: Option[String] = Option(binding)
      def bind(): Unit = {
        binding = bindingIndices.getAndIncrement().toString
      }

      val arrowsFrom: mutable.Buffer[(NodeWrapper, Option[String])] = mutable.Buffer.empty

      def bindInboundArrows(): Unit = {

        val numArrowInboundWithText = arrowsFrom
          .filter(v => v._2.nonEmpty)
          .map(_._1)
          .distinct
          .size

        if (numArrowInboundWithText >= 2) {

          arrowsFrom.foreach {
            case (v, Some(_)) => v.bind()
            case _            =>
          }
        }
      }

      final override lazy val toString = {

        val showBinding = arrowsFrom.map(_._1).distinct.size >= 2

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
          lazy val nodeText = samenessDelegatedTo.nodeText

          val ss = bindingOpt
            .map { binding =>
              s"$nodeText\n[$binding]"
            }
            .getOrElse(nodeText)
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

      val nodeID2Wrapper = PreDef
        .Fn { v =>
          NodeWrapper(v)
        }
        .cachedBy()

      val relationBuffer = mutable.Buffer.empty[(NodeWrapper, NodeWrapper)]

      val buildBuffers = AnyGraphUnary
        .^(semilattice, maxDepth)
        .Traverse(
          down = { node =>
            val wrapper = nodeID2Wrapper(node)

            val newRelations: Seq[(NodeWrapper, NodeWrapper)] = node.induction.flatMap { v =>
              v._1.arrowType match {
                case Arrow.`~>` =>
                  val to = nodeID2Wrapper.apply(v._2)
                  to.arrowsFrom += wrapper -> v._1.arrowText
                  Some(wrapper -> to)
                case Arrow.`<~` =>
                  val from = nodeID2Wrapper.apply(v._2)
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

      buildBuffers.resolve

      val nodeSet: Set[NodeWrapper] = nodeID2Wrapper.lookup.values
        .map { nodeWrapper =>
          nodeWrapper.bindInboundArrows()
          nodeWrapper
        }
        .toList
        .distinct
        .to(ListSet)
      ascii.graph.Graph(nodeSet, relationBuffer.toList)
    }

    override lazy val toString: String = {

      GraphLayout.renderGraph(asciiDiagram, layoutPrefs = _layoutPreferences)
    }
  }
}
