package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

import java.util.UUID

class Input[T](
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

object Input {
  def apply[T](defAt: SrcDefinition): Input[T] = new Input[T](defAt)

  {
    // sanity
    implicitly[Input[String] <:< Expr.Gt[Any, String]]
  }
}
