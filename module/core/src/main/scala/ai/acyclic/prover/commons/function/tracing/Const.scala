package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

case class Const[+T](value: T) extends Expr[Any, T] {
  override def getValue(
      implicit
      position: SrcDefinition
  ): T = value
}
