package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.GraphK
import ai.acyclic.prover.commons.graph.topology.Axiom.MatchSub

abstract class Topology[X <: Axiom] extends Lawful {
  self: Singleton =>

  type Graph[v] = GraphK.Aux[_Axiom, v]

  override type _Axiom = X

  implicit def assuming(
      implicit
      matching: MatchSub[X]
  ): X { type _Arrow = matching._Arrow } = Axiom.assume[X { type _Arrow = matching._Arrow }]
}

object Topology {}
