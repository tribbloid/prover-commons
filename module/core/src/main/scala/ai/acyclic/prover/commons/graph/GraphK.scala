package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Conj, Lawful}

trait GraphK[+L <: Conj] extends Lawful.Struct[L] {

  type _E <: Engine
  def engine: _E

  type Dataset[+_]

  def entriesC: Dataset[NodeK.Compat[L, Value]]
}

object GraphK {

  type Aux[+L <: Conj, V] = GraphK[L] { type Value = V }

  trait AuxT[+L <: Conj, V] extends GraphK[L] {
    type Value = V
  }

  // Acronym of "Less Than"
  type Lt[+C <: Conj, +A <: Arrow, +V] = Aux[C, _ <: V]
}
