package ai.acyclic.prover.commons.cap

trait Bound {

  type Max <: Any
  type Min >: Nothing <: Max
}
