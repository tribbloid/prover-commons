package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

import java.util.UUID

case class Var[T](
    defineAt: SrcDefinition
) extends Expr[T, T] {

  val uuid: UUID = UUID.randomUUID()

}

object Var {}
