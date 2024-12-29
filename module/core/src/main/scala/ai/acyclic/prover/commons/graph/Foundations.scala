package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.Axiom

object Foundations {

  trait Lawful {
    type _Axiom <: Axiom
  }
}
