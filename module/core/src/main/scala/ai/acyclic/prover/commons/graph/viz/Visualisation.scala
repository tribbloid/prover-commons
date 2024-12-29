package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.{Delegating, HasInner}
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.topology.Induction.AnyGraphT

object Visualisation extends HasInner {

  abstract class OfType[X <: AnyGraphT, Y <: X](
      val applicableToType: Local.GraphTypeImpl[X, Y]
  ) extends Visualisation {

    final override type Graph_/\[V] = applicableToType.Graph[V]
    final override type Node_/\[V] = applicableToType.Node[V]

    def visualiseNode[V](node: Node_/\[V]): Visualized[V] = {

      val graph: applicableToType.Graph[V] = applicableToType.makeExact[V](node)
      val result: Visualized[V] = visualise(graph)
      result
    }
  }

}

trait Visualisation {

  type Graph_/\[V] <: Local.AnyGraph[V]
  type Node_/\[V] <: Local.AnyGraph.Node[V]

  def visualise[V](data: Graph_/\[V]): Visualized[V]

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
