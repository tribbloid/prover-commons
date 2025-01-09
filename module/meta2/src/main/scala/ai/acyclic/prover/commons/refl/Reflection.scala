package ai.acyclic.prover.commons.refl

import ai.acyclic.prover.commons.meta.{HasUniverse, ITyper}

trait Reflection extends ITyper with TypeIRMixin {}

object Reflection {

  trait Runtime extends Reflection with HasUniverse.Runtime {}
  object Runtime extends Runtime

  trait CompileTime extends Reflection
}
