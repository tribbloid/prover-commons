package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Platform
import ai.acyclic.prover.commons.graph.viz.{Hasse, Hierarchy, LinkedHierarchy}

object Local extends Platform {

  override type Dataset[T] = Seq[T]
  def parallelize[T](seq: Seq[T]): Seq[T] = seq

  implicit class GraphView[V](self: Graph[V]) {

    def diagram_Hasse(
        implicit
        format: Hasse
    ): format.Viz[V] = format.Viz(self)
  }

  implicit class OutboundGraphView[V](self: Graph.Outbound[V]) {

    def diagram_linkedHierarchy(
        implicit
        group: LinkedHierarchy#Group
    ): group.Viz[V] = group.Viz(self)
  }

  implicit class UpperSemilatticeView[V](self: Semilattice.Upper[V]) {

    def diagram_hierarchy(
        implicit
        format: Hierarchy
    ): format.Viz[V] = format.Viz(self)
  }
}
