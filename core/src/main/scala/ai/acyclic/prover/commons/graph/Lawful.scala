package ai.acyclic.prover.commons.graph

trait Lawful {
  type Law_/\ <: Law

  type Node[v] = NodeK.Compat[Law_/\, v]

  type Rewriter[v] = RewriterK.Aux[Law_/\, v]
}

object Lawful {

  trait Construct[+L <: Law] {

    val law: L
    type _A = law._A

    type Value
  }
}
