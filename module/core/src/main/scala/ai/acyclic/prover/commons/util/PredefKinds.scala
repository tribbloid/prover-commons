package ai.acyclic.prover.commons.util

object PredefKinds {

  type Invar[T] = T

  type Drop1[_, T] = T

  type OptionK[T, K[_]] = Option[K[T]]
}
