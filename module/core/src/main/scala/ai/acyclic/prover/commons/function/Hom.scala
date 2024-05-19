package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.hom.HomSystem

object Hom extends HomSystem.Entry {

  import HomSystem._

  type Fn[-I, +R] = FnCompat[I, R]
  type :=>[-I, +R] = Fn[I, R]

  def Fn = HomSystem.Fn
  def :=> = Fn

  type Poly = HomSystem.Poly
  def Poly = HomSystem.Poly

  type Mono[T, -I[_] <: IUB, +R[_]] = MonoCompat[T, I, R]
  type :|~>[-I[_] <: IUB, +R[_]] = Mono[Any, I, R]

  def Mono = HomSystem.Mono
  def :|~> = Mono

  type Dependent[T, +R[_]] = DependentCompat[T, R]
  type :|->[+R[_]] = Dependent[Any, R]

  def Dependent = HomSystem.Dependent
  def :|-> = Dependent

  object Impl {
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

}
