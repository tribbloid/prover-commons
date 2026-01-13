package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

case class Const[+T](value: T) extends Expr[T] {

  final type Pending = Any

  override def getValue(
      implicit
      defAt: SrcDefinition
  ): T = value
}
