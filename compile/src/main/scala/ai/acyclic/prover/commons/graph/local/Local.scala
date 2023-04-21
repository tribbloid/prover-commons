package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Platform
import ai.acyclic.prover.commons.graph.viz.{Hasse, Hierarchy, LinkedHierarchy}

object Local extends Platform {

  override type Dataset[T] = IndexedSeq[T]
  def parallelize[T](seq: Seq[T]): Dataset[T] = seq.toIndexedSeq

  implicit class GraphView[V](val self: Graph[V]) {

    def diagram_Hasse(
        implicit
        format: Hasse
    ): format.Viz[V] = format.Viz(self)
  }

  implicit class OutboundGraphView[V](val self: Graph.Outbound[V]) {

    def diagram_linkedHierarchy(
        implicit
        group: LinkedHierarchy#Group
    ): group.Viz[V] = group.Viz(self)
  }

  implicit class UpperSemilatticeView[V](val self: Semilattice.Upper[V]) {

    {
      require(self.roots.size == 1, "NOT a semilattice!")
    }

    def root = {

      self.roots.head
    }

    def diagram_hierarchy(
        implicit
        format: Hierarchy
    ): format.Viz[V] = format.Viz(self)
  }
}
