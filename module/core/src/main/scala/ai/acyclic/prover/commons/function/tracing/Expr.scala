package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.hom.Hom

trait Expr[I, O] extends Tracer[I, O] {

  override def getValue(
      implicit
      position: SrcDefinition
  ): O = throw new ConcretizationTypeError(this, position)
}

object Expr {

  case class _1[I, O](
      primary: Hom.Fn[I, O]
  ) extends Expr[I, O] {

    override def getValue(
        implicit
        position: SrcDefinition
    ): O = ???
  }

}
