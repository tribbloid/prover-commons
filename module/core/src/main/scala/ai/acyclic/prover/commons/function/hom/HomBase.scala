package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.function.api.SystemBase

trait HomBase extends SystemBase {
  self: Singleton =>

  final type IUB = Any

  type AsInput[+V] = V
  def asInput[T](v: T): AsInput[T] = v

}
