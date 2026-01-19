package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

/**
  * Typeclass to convert a tuple of [[Input]] into their corresponding values (using their (reify) function)
  *
  * this conversion is achieved by recursively summoning itself, thus applicable to tuple of any size
  *
  * e.g. (Input[X], Input[Y], Input[Z]) => (X, Y, Z)
  *
  * Implementation should use [[ai.acyclic.prover.commons.util.TupleUnpack]] for the recursion. Do not use shapeless
  * directly. Make sure all tests are successful.
  *
  * TODO: implement both the typeclass and its test suite
  */
trait CanReifyMany[
    T // (Input[X], Input[Y], ...)
] {

  type Out // (X, Y, ...)

  def reifyMany(
      inputs: T
  )(
      implicit
      defAt: SrcDefinition
  ): Out
}

object CanReifyMany {}
