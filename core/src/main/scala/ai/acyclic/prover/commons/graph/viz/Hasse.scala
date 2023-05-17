package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.Same
import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.local.ops.GraphUnary
import ai.acyclic.prover.commons.typesetting.TextBlock
import org.scalameta.ascii
import org.scalameta.ascii.layout.GraphLayout
import org.scalameta.ascii.layout.prefs.{LayoutPrefs, LayoutPrefsImpl}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.ListSet
import scala.collection.mutable

// TODO: rename to Flow! Hasse diagram is only applicable to poset
object Hasse extends Visualisations {

  type UB[V] = Local.Graph[V]

  trait Default extends Hasse {

    override def layoutPreferences: LayoutPrefs = LayoutPrefsImpl.DEFAULT
  }
  object Default extends Default {}

  implicit lazy val default: Default.type = Default

}

trait Hasse extends Hasse.Format {

  import Hasse._
  import Local.Graph._

  lazy val maxDepth = 20

  def layoutPreferences: ascii.layout.prefs.LayoutPrefs

  final lazy val _layoutPreferences = layoutPreferences

  lazy val bindings: LazyList[String] = (0 until Int.MaxValue).to(LazyList).map(v => "" + v)

  def apply[V](s: UB[V]): Viz[V] = Viz(s)

  val sameness = Same.ByEquality.Of[Node[_]](v => v.identityKey)

  case class Viz[V](override val semilattice: UB[V]) extends TextViz[V] {

    lazy val bindingIndices = new AtomicInteger(0)

    case class NodeWrapper(override val delegate: Node[V]) extends sameness.Facade {

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
          lazy val nodeText = delegate.nodeText

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

      val nodeID2Wrapper = sameness
        .Memoize { v =>
          NodeWrapper(v)
        }

//      val nodeBuffer = mutable.Buffer.empty[NodeWrapper]

//      def wrap(node: NodeCompat[V]): NodeWrapper = {
//        val result = NodeWrapper(node)
//        nodeBuffer += result
//        result
//      }

      val relationBuffer = mutable.Buffer.empty[(NodeWrapper, NodeWrapper)]

      val buildBuffers = GraphUnary
        .^(semilattice)
        .Traverse(
          maxDepth = Hasse.this.maxDepth,
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

      val nodeSet: Set[NodeWrapper] = nodeID2Wrapper.outer.values
        .map { nodeWrapper =>
          nodeWrapper.bindInboundArrows()
          nodeWrapper
        }
        .toList
        .distinct
        .to(ListSet)
      ascii.graph.Graph(nodeSet, relationBuffer.toList)
    }

    override lazy val graphString: String = {

      GraphLayout.renderGraph(asciiDiagram, layoutPrefs = _layoutPreferences)
    }
  }
}
