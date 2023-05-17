package ai.acyclic.prover.commons.graph

trait Lawful {
  type _L <: Law

  type Node[v] = NodeK.Compat[_L, v]

  type Rewriter[v] = RewriterK.Aux[_L, v]
}

object Lawful {

  trait Construct[+L <: Law] {

    val law: L
    type _A = law._A

    type Value
  }
}
