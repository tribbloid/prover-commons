package ai.acyclic.prover.commons.graph

trait Lawful {
  type _L <: Law
  val law: _L

  type Node[v] = NodeKind.Compat[_L, v]
  type NodeEx[v] = NodeKind.AuxEx[_L, v]

  type Rewriter[v] = RewriterKind.Aux[_L, v]
}

object Lawful {

  trait Impl extends Lawful {

    type _A = law._A

    type Value
  }

  trait Construct[+L <: Law] extends Impl {

    override type _L <: L

  }

}
