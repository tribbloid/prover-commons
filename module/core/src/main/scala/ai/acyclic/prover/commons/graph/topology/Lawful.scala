package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{NodeK, RewriterK}

trait Lawful {
  type Law_/\ <: Law

  type Node[v] = NodeK.Compat[Law_/\, v]

  type Rewriter[v] = RewriterK.Aux[Law_/\, v]
}

object Lawful {

  trait Struct[+L <: Law] {

    val law: L
    type _Arrow = law._Arrow

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!
  }
}
