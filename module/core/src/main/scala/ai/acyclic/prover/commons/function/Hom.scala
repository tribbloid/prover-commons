package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.hom.System

object Hom extends System.Entry {

  import System._

  type Fn[-I, +R] = FnCompat[I, R]
  type :=>[-I, +R] = Fn[I, R]

  def Fn = System.Fn
  def :=> = Fn

  type Poly = System.Poly
  def Poly = System.Poly

  type Mono[T, -I[_], +R[_]] = MonoCompat[T, I, R]
  type :|~>[-I[_], +R[_]] = Mono[Any, I, R]

  def Mono = System.Mono
  def :|~> = Mono

  type Dependent[T, +R[_]] = DependentCompat[T, R]
  type :|->[+R[_]] = Dependent[Any, R]

  def Dependent = System.Dependent
  def :|-> = Dependent

  object Impl {
    // TODO: should be under "Hom"

    import System._

    type Fn[I, R] = FnImpl[I, R]
    //  type :=>[I, R] = FnImpl[I, R]

    type Poly = System.Poly

    type Mono[T, I[_ <: T], R[_ <: T]] = MonoImpl[T, I, R]
    //  type :|~>[I[_] , R[_]] = MonoImpl[Any, I, R]

    type Dependent[T, R[_ <: T]] = DependentImpl[T, R]
    //  type :|=>[R[_]] = DependentImpl[Any, R]
  }

}
