package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Topology.PosetT

// has a non-cyclic sanity check
//  TODO: ... in compile-time
trait Poset[N] extends Graph[N] with PosetT._Graph[N] {}

object Poset {}
