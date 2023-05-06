package ai.acyclic.prover.commons.graph

// TODO: theoretically, should already entails Arrow type

/**
  * a container of graph constraints
  */
trait Law {

  type _A <: Arrow
}
