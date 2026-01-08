package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

case class Concrete[T](value: T) extends Tracer[Unit, T] {
  override def getValue(
      implicit
      position: SrcDefinition
  ): T = value
}
