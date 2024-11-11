package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Axioms, Lawful}

trait NodeOrGraph[+X <: Axioms] extends Lawful.Structure[X] {

//  def asGraph: GraphK.Aux[X, Value]
}

trait GraphK[+X <: Axioms] extends NodeOrGraph[X] {

  type _E <: Engine // TODO: should fold into engine using dependent type
  def engine: _E

  type Batch[+_]

  def getEntries: Batch[NodeK.Compat[X, Value]]
}

object GraphK {

  type Aux[+X <: Axioms, V] = GraphK[X] { type Value = V }
  trait Aux_[+X <: Axioms, V] extends GraphK[X] { type Value = V }

  // Acronym of "Less Than"
  type Lt[+X <: Axioms, +A <: Arrow, +V] = Aux[X, ? <: V]

  def asGraph = this
}
