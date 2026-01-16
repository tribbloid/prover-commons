package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

class ConcretizationTypeError[T](
    tracer: Traceable[T],
    position: SrcDefinition
) extends Throwable {}
