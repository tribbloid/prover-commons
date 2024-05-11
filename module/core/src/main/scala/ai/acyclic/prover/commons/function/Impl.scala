package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.hom.HomSystem

object Impl extends Hom.Builder[Any, Any] {
  // TODO: should be under "Hom"

  import HomSystem._

  type Fn[I, R] = FnImpl[I, R]
//  type :=>[I, R] = FnImpl[I, R]

  type Poly = HomSystem.Poly

  type Mono[T, I[_ <: T] <: IUB, R[_ <: T]] = MonoImpl[T, I, R]
//  type :|~>[I[_] <: IUB, R[_]] = MonoImpl[Any, I, R]

  type Dependent[T, R[_ <: T]] = DependentImpl[T, R]
//  type :|=>[R[_]] = DependentImpl[Any, R]
}
