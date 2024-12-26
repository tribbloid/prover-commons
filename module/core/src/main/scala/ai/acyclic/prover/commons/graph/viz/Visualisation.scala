package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.{Delegating, HasInner}

import scala.language.implicitConversions

object Visualisation extends HasInner {

  trait OfType extends Visualisation {

    val applicableToType: Local.SubEngine[?]

    implicitly[applicableToType.type <:< Local.SubEngine[?]]

    final override type Graph_/\[V] = applicableToType.Graph[V]
    final override type Node_/\[V] = applicableToType.Node[V]

    def visualiseNode[V](node: Node_/\[V]): Visualized[V] = {

      val graph = applicableToType.makeExact[V](node)
      val result = visualise(graph)
      result
    }
  }

}

trait Visualisation {

  type Graph_/\[V] <: Local.AnyGraph[V]
  type Node_/\[V] <: Local.AnyGraph.Node[V]

  def visualise[V](data: Local.AnyGraph[V]): Visualized[V]

  trait Visualized[V] extends Delegating[Graph_/\[V]] {}

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
