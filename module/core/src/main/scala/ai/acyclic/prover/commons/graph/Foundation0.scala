package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.Axiom

object Foundation0 {

  trait Lawful {
    type _Axiom <: Axiom.Top
  }
}
