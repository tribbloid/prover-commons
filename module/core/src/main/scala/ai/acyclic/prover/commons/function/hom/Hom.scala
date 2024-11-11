package ai.acyclic.prover.commons.function.hom

object Hom extends Hom_Imp0 {

  type :=>[-I, +R] = Circuit[I, R]
  val :=> : Circuit.type = Circuit

  type :|~>[-I[_], +R[_]] = Mono[Any, I, R]
  val :|~> : Mono.type = Mono

  type :|->[+R[_]] = Dependent[Any, R]
  val :|-> : Dependent.type = Dependent

  object Impl {

    type Circuit[I, R] = Circuit.Impl[I, R]

    type Poly = Hom.Poly

    type Mono[T, I[_ <: T], R[_ <: T]] = Mono.Impl[T, I, R]

    type Dependent[T, R[_ <: T]] = Dependent.Impl[T, R]
  }
}
