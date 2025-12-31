package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

case class Concrete[T](value: T) extends Var[T] {
  override def get(
      implicit
      position: SrcDefinition
  ): T = value
}
