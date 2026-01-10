package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

import java.util.UUID

class Var[T](
    val defineAt: SrcDefinition
) extends Expr[T, T] {

  val uuid: UUID = UUID.randomUUID()
}

object Var {
  def apply[T](defineAt: SrcDefinition): Var[T] = new Var[T](defineAt)
}
