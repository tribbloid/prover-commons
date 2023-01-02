package ai.acyclic.prover.commons.graph

// has a non-diamond sanity check
//  TODO: ... in compile-time
object Tree extends GraphSystem {

  type OpsOf[+N <: Node] = Semilattice.Upper.OpsOf[N]

  trait Node extends Semilattice.Upper.Node with OpsOf[Node]

  trait Leaf extends Node {

    final override def outbound = Nil
  }
}
