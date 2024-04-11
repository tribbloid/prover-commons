package ai.acyclic.prover.commons.function

object Hom extends HomSystem.SystemView {

  import HomSystem._

  type FnAt[-I] = HomSystem.Fn[I]
  // TODO: this cause a lot of confusion
  //  should make consistent in HomSystem

  type Fn[-I, +R] = FnCompat[I, R]
  type :=>[-I, +R] = Fn[I, R]

  type Morphism[T, -I[_] <: IUB, +R[_]] = MorphismCompat[T, I, R]
  type :|~>[-I[_] <: IUB, +R[_]] = MorphismCompat[Any, I, R]

  type Dependent[T, +R[_]] = DependentCompat[T, R]
  type :|=>[+R[_]] = DependentCompat[Any, R]

  type Poly = HomSystem.Poly
}
