package ai.acyclic.prover.commons.graph

trait Lawful {
  type _L <: Law

  type Node[v] = NodeKind.Compat[_L, v]
  type NodeEx[v] = NodeKind.AuxEx[_L, v]

  type Rewriter[v] = RewriterKind.Aux[_L, v]

  trait _Construct extends Lawful.Construct[this._L]
}

object Lawful {

  trait Construct[+L <: Law] {

    val law: L
    type _A = law._A

    type Value
  }

}
