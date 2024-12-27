package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.Induction

object Foundations {

  trait Lawful {
    type _Axiom <: Induction
  }
}
