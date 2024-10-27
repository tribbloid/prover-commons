package ai.acyclic.prover.commons.function.hom

object Hom extends Hom_Imp0 {

  type :=>[I, R] = Circuit.Impl[I, R]
  def :=> = Circuit

  type :|~>[I[_], R[_]] = Mono.Impl[Any, I, R]
  def :|~> = Mono

  type :|->[R[_]] = Dependent.Impl[Any, R]
  def :|-> = Dependent

//  object Impl {
//
//    type Circuit[I, R] = Circuit.Impl[I, R]
//  }
}
