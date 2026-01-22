package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.multiverse.rewrite.HasConversionPart

import scala.language.implicitConversions

trait FnImp0 extends FnImp1 with HasConversionPart {
  self: Expr.type =>

  implicit def _forInput_<-[I, O](
      self: TracingFn[I, O]
  )(
      implicit
      canReify: CanReifyMany.Aux[O, O]
  ): ForComprehensions[I, O, O] = ForComprehensions(self, canReify)

  implicit def tuple2ToFn[I1, O1, I2, O2]: (TracingFn[I1, O1], TracingFn[I2, O2]) ?++>
    TracingFn[(I1, I2), (O1, O2)] = { tuple =>
    val (f1, f2) = tuple

    val result = Hom.Fn.Pointwise(f1.concrete, f2.concrete)

    TracingFn(result)
  }

}
