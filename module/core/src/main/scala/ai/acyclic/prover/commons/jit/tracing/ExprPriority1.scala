package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

trait ExprPriority1 {
  implicit def _getValue[T](v: Expr.Gt[?, T])(
      implicit
      defAt: SrcDefinition = null
  ): T =
    v.reify
}
