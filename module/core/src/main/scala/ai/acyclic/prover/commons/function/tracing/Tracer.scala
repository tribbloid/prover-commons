package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

case class Tracer[T](
    defineAt: SrcDefinition
) extends Var[T] {

  import Tracer.NoConcreteValue

  override def get(
      implicit
      position: SrcDefinition
  ): T = throw new NoConcreteValue(this, position)
}

object Tracer {

  class NoConcreteValue[T](
      tracer: Tracer[T],
      position: SrcDefinition
  ) extends Throwable {}
}
