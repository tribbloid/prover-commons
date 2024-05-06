package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.function.api.HasMono

trait HomBase extends HasMono with Serializable {

  final type IUB = Any

  type AsInput[+V] = V
  def asInput[T](v: T): AsInput[T] = v

}
