package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow

/**
  * a container of graph constraints
  */
trait Axiom {

  type _Arrow <: Arrow
}

object Axiom {

  object ^ extends Axiom

  def apply[T <: Axiom]: T = ^.asInstanceOf[T]

  trait Impl[+A <: Arrow] extends Axiom { type _Arrow <: A }
}
