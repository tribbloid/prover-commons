package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{NodeK, RewriterK}

trait Lawful {
  type _Axiom <: Induction

  final type Node[V] = NodeK.Lt[_Axiom, V]

  final type Rewriter[V] = RewriterK.Aux[_Axiom, V]

}

object Lawful {

  trait Refined extends Lawful {

//    val axioms: X
//    final type _Arrow = axioms._Arrow

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!

    type NodeV = NodeK.Lt[_Axiom, Value]

    type RewriterV = RewriterK.Aux[_Axiom, Value]
  }
}
