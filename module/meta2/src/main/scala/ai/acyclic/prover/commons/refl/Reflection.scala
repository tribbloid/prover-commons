package ai.acyclic.prover.commons.refl

import ai.acyclic.prover.commons.Delegating
import ai.acyclic.prover.commons.meta.{HasUniverse, ITyper}
import ai.acyclic.prover.commons.viz.format.TypeFormat

import scala.language.implicitConversions

trait Reflection extends ITyper with TypeIRMixin {

  case class _FormattingExt(unbox: TypeView) extends Delegating[TypeView] {

    def formattedBy(format: TypeFormat): TypeIR = {
      val result = TypeIR(this, format)
      result.text
      result
    }
  }
}

object Reflection {

  implicit def _formattingOps(
      unbox: rr.TypeView forSome {
        val rr: TypeIRMixin & Reflection
      }
  ): unbox.outer._FormattingExt = {
    unbox.outer._FormattingExt(unbox.asInstanceOf[unbox.outer.TypeView]) // fuck scala
  }

  trait Runtime extends Reflection with HasUniverse.Runtime {}
  object Runtime extends Runtime

  trait CompileTime extends Reflection
}
