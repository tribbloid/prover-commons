package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.Same

trait GraphKind[+C <: Topology.Constraint, +A <: Arrow, V] extends GraphKind.Like {

  final type Value = V

  def roots: platform.Dataset[NodeKind[C, A, V]]
}

object GraphKind {

  trait Like {

    val platform: Platform

    // should only compare the sameness of node, NOT value!
    //  otherwise a mapping to the value may introduce forbidden subgraph(s).
    lazy val sameness: Same.Definition = Same.ByEquality
  }

  // TODO: don't know how to define it in the new architecture`
//  trait Immutable[+Ops <: Node.Like[_, _]] extends GraphK[Ops] {
//
//    // the Memoization appears to confine GraphK to be only applicable to immutable graph
//    //  can this be lifted?
//    override lazy val ops: Value => Ops =
//      sameness
//        .Memoize[Value, Ops](
//          Ops
//        )
//  }
}
