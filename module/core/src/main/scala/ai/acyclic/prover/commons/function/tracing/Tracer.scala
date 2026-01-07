package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

import java.util.UUID

class Tracer[T](
    defineAt: SrcDefinition
) extends Var[T] {

  val uuid: UUID = UUID.randomUUID()

  import Tracer.ConcretizationTypeError

  override def get(
      implicit
      position: SrcDefinition
  ): T = throw new ConcretizationTypeError(this, position)
}

object Tracer {

  class ConcretizationTypeError[T](
      tracer: Tracer[T],
      position: SrcDefinition
  ) extends Throwable {}
}
