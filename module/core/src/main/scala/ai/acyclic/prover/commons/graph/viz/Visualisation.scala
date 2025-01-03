package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.{Delegating, HasInner}
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.topology.Axiom.Top

object Visualisation extends HasInner { // TODO: should be a mixin

  abstract class OfType[X <: Top](
      val applicableToType: Local.GraphType[X]
  ) extends Visualisation {

    override type MaxGraph[V] = applicableToType.Graph[V]
    type MaxNode[V] = applicableToType.Node[V]

    def showNode[V](node: MaxNode[V]): Visual[V] = {

      val graph: applicableToType.Graph[V] = applicableToType.makeExact[V](node)
      val result: Visual[V] = show(graph)
      result
    }
  }
}

trait Visualisation {

  type MaxGraph[V] <: Local.AnyGraph[V]

  type Visual[V] = Delegating[MaxGraph[V]] {}
  def show[V](data: MaxGraph[V]): Visual[V]

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
