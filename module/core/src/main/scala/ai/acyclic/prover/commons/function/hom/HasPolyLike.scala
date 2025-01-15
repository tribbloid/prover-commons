package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.function.Traceable

trait HasPolyLike extends HasCircuit {

  trait PolyLike extends Serializable with Traceable {}
}
