package ai.acyclic.prover.commons.refl

import ai.acyclic.prover.commons.Delegating
import ai.acyclic.prover.commons.meta.{HasUniverse, ITyper}
import ai.acyclic.prover.commons.viz.format.TypeFormat

trait Reflection extends ITyper with TypeIRMixin {

  case class TypeOps(unbox: TypeView) extends Delegating[TypeView] {

    def formattedBy(format: TypeFormat): TypeIR = {
      val result = TypeIR(this, format)
      result.text
      result
    }
  }
}

object Reflection {

  trait Runtime extends Reflection with HasUniverse.Runtime {}
  object Runtime extends Runtime

  trait CompileTime extends Reflection
}
