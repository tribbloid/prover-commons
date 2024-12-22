package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{NodeK, RewriterK}

trait Lawful {
  type _Axiom <: Induction

  type Node[v] = NodeK.Compat[_Axiom, v]

  type Rewriter[v] = RewriterK.Aux[_Axiom, v]

}

object Lawful {

  trait Structure[+X <: Induction] extends Lawful {

    override type _Axiom <: X

    val axioms: X
    final type _Arrow = axioms._Arrow

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!
  }
}
