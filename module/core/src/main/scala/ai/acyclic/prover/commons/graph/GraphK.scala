package ai.acyclic.prover.commons.graph

trait GraphK[+L <: Law] extends Lawful.Struct[L] {

  type _E <: Engine
  def engine: _E

  type Dataset[+_]

  def entriesC: Dataset[NodeK.Compat[L, Value]]
}

object GraphK {

  type Aux[+L <: Law, V] = GraphK[L] { type Value = V }

  trait AuxT[+L <: Law, V] extends GraphK[L] {
    type Value = V
  }

  // Acronym of "Less Than"
  type Lt[+C <: Law, +A <: Arrow, +V] = Aux[C, _ <: V]
}
