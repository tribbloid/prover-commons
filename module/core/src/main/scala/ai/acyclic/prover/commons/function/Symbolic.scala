package ai.acyclic.prover.commons.function

import shapeless.HNil

object Symbolic {
  import shapeless.::

  type :=[+R] = T0.Function[HNil, R]

  type :=>[T, +R] = T1.Function[T :: HNil, R]
}
