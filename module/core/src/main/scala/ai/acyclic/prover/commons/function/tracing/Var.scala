package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

import java.util.UUID

class Var[T](
    val defineAt: SrcDefinition,
    val inhabited: Option[Inhabited[T]] = None
) extends Expr[T, T] {

  val uuid: UUID = UUID.randomUUID()

  override def getValue(
      implicit
      position: SrcDefinition
  ): T = inhabited
    .map(_.getExample)
    .getOrElse(
      super.getValue
    )
}

object Var {
  def apply[T](defineAt: SrcDefinition): Var[T] = new Var[T](defineAt)
}
