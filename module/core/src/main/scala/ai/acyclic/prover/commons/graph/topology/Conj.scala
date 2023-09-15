package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow

/**
  * a container of graph constraints
  */
trait Conj {

  type _Arrow <: Arrow
}

object Conj {

  object ^ extends Conj

  def apply[T <: Conj]: T = ^.asInstanceOf[T]

  trait Impl[+A <: Arrow] extends Conj { type _Arrow <: A }
}
