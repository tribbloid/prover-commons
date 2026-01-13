package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

import java.util.UUID

class Var[T](
    val defAt: SrcDefinition,
    val inhabited: Option[Inhabited[T]] = None
) extends Expr[T] {

  final type Pending = T

  val uuid: UUID = UUID.randomUUID()

  override def getValue(
      implicit
      defAt: SrcDefinition
  ): T = inhabited
    .map(_.getExample)
    .getOrElse(
      super.getValue
    )
}

object Var {
  def apply[T](defAt: SrcDefinition): Var[T] = new Var[T](defAt)
}
