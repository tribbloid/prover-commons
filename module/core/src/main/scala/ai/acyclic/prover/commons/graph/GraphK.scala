package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Axiom, Lawful}

trait GraphK[+X <: Axiom] extends Lawful.Struct[X] {

  type _E <: Engine
  def engine: _E

  type Dataset[+_]

  def entriesC: Dataset[NodeK.Compat[X, Value]]
}

object GraphK {

  type Aux[+X <: Axiom, V] = GraphK[X] { type Value = V }

//  trait AuxT[+L <: Axiom, V] extends GraphK[L] {
//    type Value = V
//  }

  // Acronym of "Less Than"
  type Lt[+X <: Axiom, +A <: Arrow, +V] = Aux[X, _ <: V]
}
