package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{NodeK, RewriterK}

trait Lawful {
  type _Axiom <: Axiom

  type Node[v] = NodeK.Compat[_Axiom, v]

  type Rewriter[v] = RewriterK.Aux[_Axiom, v]

}

object Lawful {

  trait Struct[+X <: Axiom] extends Lawful {

    override type _Axiom <: X

    val assuming: X
    type _Arrow = assuming._Arrow

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!
  }
}
