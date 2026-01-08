package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

case class InhabitedVar[T](
    defaultValue: T,
    defineAt: SrcDefinition
) extends Tracer[Unit, T] {

  override def getValue(
      implicit
      position: SrcDefinition
  ): T = defaultValue
}
