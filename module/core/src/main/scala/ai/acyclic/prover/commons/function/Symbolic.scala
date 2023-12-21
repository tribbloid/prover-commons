package ai.acyclic.prover.commons.function

import shapeless.{::, HNil}

trait Symbolic {

  type :=[R] = T0.Fn[HNil, R]
  type Fn0[R] = :=[R]

  type :=>[I, R] = T1.Fn[I :: HNil, R]
  type Fn1[I, R] = :=>[I, R]

  type :|=>[-I, +R[_]] = T1.Dependent[I :: HNil, R]

  type :|~>[-I[_], +R[_]] = T1.Morphism[Lambda[t => I[t] :: HNil], R]

  type Poly1 = T1.Poly

}

object Symbolic extends Symbolic {}
