package ai.acyclic.prover.commons.function.hom

object Hom extends Hom_Imp0 {

  type :=>[-I, +R] = Circuit[I, R]
  val :=> : Circuit.type = Circuit

  type :|~>[-I[_], +R[_]] = Poly1[Any, I, R]
  val :|~> : Poly1.type = Poly1

  type :|->[+R[_]] = Dependent[Any, R]
  val :|-> : Dependent.type = Dependent

  object Impl {

    type Circuit[I, R] = Circuit.Impl[I, R]

    type Poly = Hom.Poly

    type Poly1[T, I[_ <: T], R[_ <: T]] = Poly1.Impl[T, I, R]

    type Dependent[T, R[_ <: T]] = Dependent.Impl[T, R]
  }
}
