package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.HasOuter
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.viz.Visualisation.HasData

import scala.language.implicitConversions

object Visualisation {

  trait HasData[G] extends HasOuter {

    val data: G

    //    override lazy val toString: String = "[MISSING]"
  }

  object HasData {

    implicit def unbox[G <: Local.AnyGraph[?]](v: HasData[G]): G = v.data
  }

}

trait Visualisation {

  val graphType: Local.GraphType[_, _]

  final type Graph_/\[V] = graphType.Graph[V]
  final type Node_/\[V] = graphType.Node[V]

  def visualise[V](data: Graph_/\[V]): Visualized[V]

  def visualiseNode[V](node: Node_/\[V]): Visualized[V] = {

    val graph = graphType.makeExact[V](node)
    visualise(graph)
  }

  trait Visualized[V] extends HasData[Graph_/\[V]] {

    override val outer: Visualisation.this.type = Visualisation.this
  }

//  trait Extensions {
//
//    implicit class GraphView[V](graph: Graph_/\[V]) {
//
//      def show = Visualisation.this.visualise(graph)
//    }
//
//    implicit class NodeView[V](node: Node_/\[V]) {
//
//      def show = Visualisation.this.visualise(node.asGraph)
//    }
//  }

}
