package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow

/**
  * a container of graph constraints
  */
trait Law {

  type _Arrow <: Arrow
}

object Law {

  type Aux[A] = Law { type _Arrow = A }
  trait Impl[+A <: Arrow] extends Law { type _Arrow <: A }
}
