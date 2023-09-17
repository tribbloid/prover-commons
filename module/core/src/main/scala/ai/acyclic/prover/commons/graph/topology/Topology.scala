package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.topology.Axiom.MatchSub
import ai.acyclic.prover.commons.graph.{Arrow, GraphK}

trait Topology[X <: Axiom] extends Lawful {

  override type Axiom_/\ = X
  type _Arrow <: Arrow
  type _Axiom = Axiom_/\ { type _Arrow = Topology.this._Arrow }

  final def axiom: _Axiom = Axiom.apply[_Axiom]
}

object Topology {

  abstract class HasTopology[X <: Axiom] extends Lawful {
    self: Singleton =>

    type Graph[v] = GraphK.Aux[Axiom_/\, v]

    override type Axiom_/\ = X

    abstract class _Topology extends Topology[X] {}

    implicit def top(
        implicit
        matching: MatchSub[X]
    ): _Topology { type _Arrow = matching._Arrow } = new _Topology {
      override type _Arrow = matching._Arrow
    }
  }

}
