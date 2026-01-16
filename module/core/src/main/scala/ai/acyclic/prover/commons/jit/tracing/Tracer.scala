package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

import java.util.UUID

class Tracer[T](
    val defAt: SrcDefinition,
    val inhabited: Option[Inhabited[T]] = None
) extends Expr[T] {

  type Pending = T

  val uuid: UUID = UUID.randomUUID()

  override def getConcrete(
      implicit
      defAt: SrcDefinition
  ): T = inhabited
    .map(_.getExample)
    .getOrElse(
      super.getConcrete
    )
}

object Tracer {
  def apply[T](defAt: SrcDefinition): Tracer[T] = new Tracer[T](defAt)

}
