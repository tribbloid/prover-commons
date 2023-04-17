package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local.Local.Dataset
import ai.acyclic.prover.commons.{HasOuter, Same}

trait Structure[+C <: Topology.Constraint, +A <: Arrow, V] extends Structure.Like {

  final type Value = V

  def roots: Dataset[Node[C, A, V]]
}

object Structure {

  trait Like extends HasOuter {

    type Value

    val platform: Platform
    final def outer: Platform = platform

    // should only compare the sameness of node, NOT value!
    //  otherwise a mapping to the value may introduce forbidden subgraph(s).
    lazy val sameness: Same.Definition = Same.ByEquality

//    type Dataset[T] = platform.Dataset[T]
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
