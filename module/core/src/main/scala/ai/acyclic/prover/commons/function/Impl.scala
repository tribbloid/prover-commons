package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.debug.Debug.CallStackRef
import ai.acyclic.prover.commons.function.hom.HomSystem

object Impl extends HomSystem.SystemView {
  // TODO: should be under "Hom"

  import HomSystem._

  type Fn[I, R] = FnImpl[I, R]
//  type :=>[I, R] = FnImpl[I, R]

  def apply[I <: IUB, R](
      fn: I => R
  )(
      implicit
      _definedAt: CallStackRef = definedHere
  ): Fn[I, R] = {
    Fn(fn)
  }

  type Poly = HomSystem.Poly

  type Mono[T, I[_ <: T] <: IUB, R[_ <: T]] = MonoImpl[T, I, R]
//  type :|~>[I[_] <: IUB, R[_]] = MonoImpl[Any, I, R]

  type Dependent[T, R[_ <: T]] = DependentImpl[T, R]
//  type :|=>[R[_]] = DependentImpl[Any, R]
}
