package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.graph.processing.GraphPlans
import org.scalameta.ascii
import org.scalameta.ascii.layout.GraphLayout
import org.scalameta.ascii.layout.prefs.{LayoutPrefs, LayoutPrefsImpl}

import scala.collection.mutable

object Hasse extends Visualisations {

  type UB[N] = Graph[N]

  trait Default extends Hasse {

    override def layoutPreferences: LayoutPrefs = LayoutPrefsImpl.DEFAULT
  }
  object Default extends Default {}

  implicit lazy val default = Default

}

trait Hasse extends Hasse.Format {

  import Hasse._

  lazy val maxDepth = 20

  def layoutPreferences: ascii.layout.prefs.LayoutPrefs

  final lazy val _layoutPreferences = layoutPreferences

  def apply[N](s: UB[N]): Viz[N] = Viz(s)

  case class Viz[N](override val graph: UB[N]) extends TextViz[N] {

    case class TextWrapper(node: N) {

      lazy val nOps: graph.Ops = graph.nodeOps(node)
      final override lazy val toString = nOps.nodeText
    }

    lazy val nodeBuffer = mutable.Buffer.empty[TextWrapper]
    lazy val relationBuffer = mutable.Buffer.empty[(TextWrapper, TextWrapper)]

    lazy val buildBuffers = GraphPlans(graph)
      .Traverse(
        maxDepth = Hasse.this.maxDepth,
        down = { node =>
          val wrapper = TextWrapper(node)
          nodeBuffer += wrapper

          val newRelations = wrapper.nOps.induction.flatMap { arrow =>
            arrow.arrowType match {
              case Arrow.`~>` => Some(wrapper -> TextWrapper(arrow.target))
              case Arrow.`<~` => Some(TextWrapper(arrow.target) -> wrapper)
              case _          => None
            }
          }

          relationBuffer ++= newRelations
        }
      )
      .DepthFirst_ForEach

    lazy val asciiDiagram = {

      buildBuffers.exeOnce
      ascii.graph.Graph(nodeBuffer.toSet, relationBuffer.toList)
    }

    override def treeString: String = {

      GraphLayout.renderGraph(asciiDiagram, layoutPrefs = _layoutPreferences)
    }
  }
}
