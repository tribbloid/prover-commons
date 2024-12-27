package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Refinement
import ai.acyclic.prover.commons.graph.topology.Induction.ExtractArrow

abstract class Topology[X <: Induction] extends Refinement.Lawful {
  self: Singleton =>

  type Graph[v] = Refinement.GraphK.Aux[_Axiom, v]

  override type _Axiom = X

  implicit def assuming(
      implicit
      extractArrow: ExtractArrow.Gt[X] // TODO: how to remove this crap?
  ): X { type _Arrow = extractArrow._Arrow } = Induction.assume[X { type _Arrow = extractArrow._Arrow }]
}

object Topology {}
