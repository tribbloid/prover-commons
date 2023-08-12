package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.law.Law

trait Lawful {
  type Law_/\ <: Law

  type Node[v] = NodeK.Compat[Law_/\, v]

  type Rewriter[v] = RewriterK.Aux[Law_/\, v]
}

object Lawful {

  trait Construct[+L <: Law] {

    val law: L
    type _A = law._A

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!
  }
}
