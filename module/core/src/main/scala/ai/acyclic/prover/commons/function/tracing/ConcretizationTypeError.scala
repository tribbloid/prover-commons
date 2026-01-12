package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

class ConcretizationTypeError[T](
    tracer: Tracer[T],
    position: SrcDefinition
) extends Throwable {}
