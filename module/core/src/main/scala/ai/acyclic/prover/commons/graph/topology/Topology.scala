package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Refinement
import ai.acyclic.prover.commons.graph.topology.Axiom.ExtractArrow

abstract class Topology[X <: Axiom] extends Refinement.Lawful {
  self: Singleton =>

  type Graph[v] = Refinement.GraphK.Aux[_Axiom, v]

  override type _Axiom = X

  implicit def assuming(
      implicit
      extractArrow: ExtractArrow.Gt[X] // TODO: how to remove this crap?
  ): X { type _Arrow = extractArrow._Arrow } = Axiom.assume[X { type _Arrow = extractArrow._Arrow }]
}

object Topology {}
