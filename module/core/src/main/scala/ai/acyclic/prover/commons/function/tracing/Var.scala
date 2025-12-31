package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

trait Var[T] {

  def get(
      implicit
      position: SrcDefinition
  ): T
}

object Var {

  sealed trait Expression[T] extends Var[T] {}

  trait Expr1[T] extends Expression[T] {

    val base: Var[?]
  }

  // CAUTION: do not add Expr2[T] unless absolutely necessary
  // all reduction rules should be defined for curried form that yields higher order function(s)
}
