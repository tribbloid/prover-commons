package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Topology.SemilatticeT
import ai.acyclic.prover.commons.graph.viz.Hierarchy

trait Semilattice[N] extends Poset[N] with SemilatticeT._Graph[N] {

  def root: N

  final lazy val roots: Dataset[N] = sys.parallelize(Seq(root))
}

object Semilattice {

  trait Upper[N] extends Semilattice[N] with Graph.Outbound[N] with SemilatticeT.UpperT._Graph[N] {

    def diagram_hierarchy(
        implicit
        format: Hierarchy
    ): format.Viz[N] = format.Viz(this)
  }

  object Upper {}
}
