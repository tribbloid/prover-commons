package ai.acyclic.prover.commons.jit.poly

import ai.acyclic.prover.commons.Casting.Coerced

/**
  * dependent sigma type, armed with an implicit coersion from T to Repr[T]
  *
  * for any instance of DepSigma, any function that requires a Repr[T] can be applied on T (if DepRight[T] exists)
  */
trait DepSigma { // dependent sigma type

  type Left

  type DepRight[T <: Left]

  case class Repr[T <: Left](
      left: T,
      right: DepRight[T]
  ) extends Coerced

  implicit def pack[T <: Left](v: T)(
      implicit
      ev: DepRight[T]
  ): Repr[T] = Repr(v, ev)
}
