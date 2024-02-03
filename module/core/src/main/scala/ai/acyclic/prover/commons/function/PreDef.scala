package ai.acyclic.prover.commons.function

object PreDef extends System.SystemView {

  import System._

  type Fn[I, R] = FnCompat[I, R]
  type :=>[I, R] = FnCompat[I, R]

  type Morphism[T, -I[_] <: IUB, +R[_]] = MorphismCompat[T, I, R]
  type :|~>[-I[_] <: IUB, +R[_]] = MorphismCompat[Any, I, R]

  type Dependent[T, +R[_]] = DependentCompat[T, R]
  type :|=>[+R[_]] = DependentCompat[Any, R]

  type Poly = System.Poly
}
