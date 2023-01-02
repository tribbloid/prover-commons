package ai.acyclic.prover.commons.graph.local

// has a non-cyclic sanity check
//  TODO: ... in compile-time
trait Poset[N] extends Graph[N] {

  override val outer: Local._GraphType = Poset
}

object Poset extends Local._GraphType
