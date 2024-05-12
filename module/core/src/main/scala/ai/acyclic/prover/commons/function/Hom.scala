package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.hom.HomSystem

object Hom extends HomSystem.API {

  import HomSystem._

  type Fn[-I, +R] = FnCompat[I, R]
  type :=>[-I, +R] = Fn[I, R]

  type Mono[T, -I[_] <: IUB, +R[_]] = MonoCompat[T, I, R]
  type :|~>[-I[_] <: IUB, +R[_]] = MonoCompat[Any, I, R]

  type Dependent[T, +R[_]] = DependentCompat[T, R]
  type :|=>[+R[_]] = DependentCompat[Any, R]

  type Poly = HomSystem.Poly

}
