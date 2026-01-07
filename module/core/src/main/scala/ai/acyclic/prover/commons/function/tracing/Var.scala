package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.multiverse.rewrite.Conversion

trait Var[T] {

  def get(
      implicit
      position: SrcDefinition
  ): T
}

object Var {

  implicit def _get[T](
      implicit
      position: SrcDefinition
  ): Conversion[Var[T], T] =
    (v: Var[T]) => v.get(position)

  sealed trait Expression[T] extends Var[T] {}

  trait Expr1[T] extends Expression[T] {

    val base: Var[?]
  }

  // CAUTION: do not add Expr2[T] unless absolutely necessary
  // all reduction rules should be defined for curried form that yields higher order function(s)
}
