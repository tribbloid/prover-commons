package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow

/**
  * a container of graph constraints
  */
trait Law {

  type _Arrow <: Arrow
}
