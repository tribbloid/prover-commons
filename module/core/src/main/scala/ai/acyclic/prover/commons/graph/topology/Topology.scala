package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.GraphK
import ai.acyclic.prover.commons.graph.topology.Axioms.ImplUnpack

abstract class Topology[X <: Axioms] extends Lawful {
  self: Singleton =>

  type Graph[v] = GraphK.Aux[_Axiom, v]

  override type _Axiom = X

  implicit def assuming(
      implicit
      unpack: ImplUnpack.Gt[X]
  ): X { type _Arrow = unpack._Arrow } = Axioms.assume[X { type _Arrow = unpack._Arrow }]
}

object Topology {}
