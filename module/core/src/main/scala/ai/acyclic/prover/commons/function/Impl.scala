package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.debug.Debug.CallStackRef

object Impl extends System.SystemView {
  // TODO: should be under "Hom"

  import System._

  type Fn[I, R] = FnImpl[I, R]
  type :=>[I, R] = FnImpl[I, R]

  def apply[I <: IUB, R](
      fn: I => R
  )(
      implicit
      _definedAt: CallStackRef = definedHere
  ): Fn[I, R] = {
    Fn(fn)
  }

  type Morphism[T, I[_ <: T] <: IUB, R[_ <: T]] = MorphismImpl[T, I, R]
  type :|~>[I[_] <: IUB, R[_]] = MorphismImpl[Any, I, R]

  type Dependent[T, R[_ <: T]] = DependentImpl[T, R]
  type :|=>[R[_]] = DependentImpl[Any, R]

  type Poly = System.Poly
}
