package ai.acyclic.prover.commons.graph.local

// has a non-diamond sanity check
//  TODO: ... in compile-time
trait Tree[N] extends Semilattice.Upper[N] {

  override val outer: Local._GraphType = Tree
}

object Tree extends Local._GraphType {}
