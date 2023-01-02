package ai.acyclic.prover.commons.graph

// has a non-cyclic sanity check
//  TODO: ... in compile-time
object Poset extends GraphSystem {

  type OpsOf[+N <: Node] = Graph.OpsOf[N]

  trait Node extends Graph.Node with Graph.OpsOf[Node]
}
