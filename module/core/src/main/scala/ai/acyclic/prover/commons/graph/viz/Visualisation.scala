package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.HasOuter
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.topology.Axioms.AnyGraphT
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

  trait OfType extends Visualisation {

    val applicableToType: Local.GraphTypeImpl[_ <: AnyGraphT, _ <: AnyGraphT]

    implicitly[applicableToType.type <:< Local.GraphTypeImpl[_, _]]

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

  def visualise[V](data: Graph_/\[V]): Visualized[V]

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
