package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.multiverse.rewrite.HasConversionPart

import scala.language.implicitConversions

trait FnImp1 extends FnCanChain with HasConversionPart {

  implicit def tuple2ToFn[I1, O1, I2, O2]: (StaticTracingFn[I1, O1], StaticTracingFn[I2, O2]) ?++>
    StaticTracingFn[(I1, I2), (O1, O2)] = { v =>
    ???
  }

  implicit class UnaryForComprehensions[I, O](
      private val self: StaticTracingFn[I, O]
  ) {

    // minimal requirement for for-comprehension
    def map[OO](right: Input[O] => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): StaticTracingFn[I, canChain.Repr] = {
      ???
    }

    def foreach(right: Input[O] => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): StaticTracingFn[I, Unit] = {
      ???
    }

    def flatMap[I2, OO](right: Input[O] => StaticTracingFn[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): StaticTracingFn[(I, I2), canChain.Repr] = {
      ???
    }

    def withFilter(right: Input[O] => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): StaticTracingFn[I, O] = {

      ???
    }
  }

  // TODO: the following can be removed by carefully using ConversionPart
  implicit def tuple2ToOps1[I1, O1, I2, O2]: (StaticTracingFn[I1, O1], StaticTracingFn[I2, O2]) ?++>
    UnaryForComprehensions[(I1, I2), (O1, O2)] = {
    ???
  }
}
