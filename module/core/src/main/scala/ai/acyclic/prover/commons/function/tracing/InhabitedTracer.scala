package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

case class InhabitedTracer[T](
    defaultValue: T,
    defineAt: SrcDefinition
) extends Tracer[T](defineAt) {

  override def get(
      implicit
      position: SrcDefinition
  ): T = defaultValue
}
