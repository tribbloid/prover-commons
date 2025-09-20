package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.topology.Axiom
import ai.acyclic.prover.commons.graph.viz.{Flow, Hierarchy, LinkedHierarchy}

abstract class VisualOps[X <: Axiom.Top, V](
    graph: Local.Graph[X, V]
) {

  def text_flow(
      viz: Flow = Flow.Default
  ): viz.Visual = {

    viz.show[V](graph)
  }

  def text_linkedHierarchy(
      viz: LinkedHierarchy = LinkedHierarchy.Default
  )(
      implicit
      < : Local.Graph[X, V] <:< Local.Diverging.Graph[V]
  ): viz.Visual = {

    viz.show[V](graph)
  }

  def text_hierarchy(viz: Hierarchy = Hierarchy.Default)(
      implicit
      < : Local.Graph[X, V] <:< Local.Diverging.UpperSemilattice[V]
  ): viz.Visual = {

    viz.show[V](graph)
  }
}
