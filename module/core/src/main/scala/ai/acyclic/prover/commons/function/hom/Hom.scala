package ai.acyclic.prover.commons.function.hom

object Hom extends Hom_Imp0 {

  type :=>[-I, +R] = Circuit[I, R]
  def :=> = Circuit

  type :|~>[-I[_], +R[_]] = Mono[Any, I, R]
  def :|~> = Mono

  type :|->[+R[_]] = Dependent[Any, R]
  def :|-> = Dependent

  object Impl {

    type Circuit[I, R] = Circuit.Impl[I, R]

    type Poly = Hom.Poly

    type Mono[T, I[_ <: T], R[_ <: T]] = MonoImpl[T, I, R]

    type Dependent[T, R[_ <: T]] = DependentImpl[T, R]
  }
}
