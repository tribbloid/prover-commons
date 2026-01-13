package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

class ConcretizationTypeError[T](
    tracer: Tracer[T],
    position: SrcDefinition
) extends Throwable {}
