package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Topology.TreeT

// has a non-diamond sanity check
//  TODO: ... in compile-time
trait Tree[N] extends Semilattice.Upper[N] with TreeT._Graph[N] {}

object Tree {}
