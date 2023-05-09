package ai.acyclic.prover.commons.graph

trait Lawful {
  type _L <: Law

  type NodeCompat[v] = NodeKind.Compat[_L, v]
//  type NodeEx[v] = NodeKind.AuxEx[_L, v]

  type RewriterCompat[v] = RewriterKind.Aux[_L, v]
//  type RewriterEx[v] = RewriterKind.AuxEx[_L, v]

  trait Construct extends Lawful.ConstructKind[this._L]
}

object Lawful {

  trait ConstructKind[+L <: Law] {

    val law: L
    type _A = law._A

    type Value
  }

}
