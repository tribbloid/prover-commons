package ai.acyclic.prover.infra

import ai.acyclic.prover.infra.DuckSubtyping.{AA, BB}

object DuckSubtyping {

  trait A[T] {
    type Value
    type Cat
  }

  trait B[T] extends A[T]

  type AA[V] = A[?] { type Value <: V }
  type BB[V] = B[?] { type Value <: V }

  implicitly[BB[Int] <:< AA[Int]]

  trait K {

    type C

    type AA[V] = A[?] { type Value <: V; type Cat <: C }
    type BB[V] = B[?] { type Value <: V; type Cat <: C }

    implicitly[BB[Int] <:< AA[Int]]
  }
}
