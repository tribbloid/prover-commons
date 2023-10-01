package ai.acyclic.prover.commons.graph.local.dsl

import ai.acyclic.prover.commons.graph.local.LocalEngine
import ai.acyclic.prover.commons.graph.topology.Axiom

case class Composable[+X <: Axiom](
) extends LocalEngine._Struct[X] {

  override val assuming: X = ???
  override type Value = this.type
}
