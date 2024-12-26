package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Induction, Lawful}

trait NodeOrGraph[+X <: Induction] extends Lawful.Refined {

//  def asGraph: GraphK.Aux[X, Value]

}

trait GraphK[+X <: Induction] extends NodeOrGraph[X] {

  type _E <: Engine // TODO: should fold into engine using dependent type

  def engine: _E

  type Batch[+_]

  def entries: Batch[NodeV]

  override type _Axiom <: X
}

object GraphK {

  type Aux[+X <: Induction, V] = GraphK[X] { type Value = V }
  trait Aux_[+X <: Induction, V] extends GraphK[X] { type Value = V }

  // Acronym of "Less Than"
  type Lt[+X <: Induction, +A <: Arrow, +V] = Aux[X, ? <: V]

  def asGraph = this
}
