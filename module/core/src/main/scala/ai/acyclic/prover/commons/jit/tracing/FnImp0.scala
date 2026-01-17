package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

import scala.language.implicitConversions

trait FnImp0 extends FnImp1 {

  implicit def tuple2ToFn[I1, O1, I2, O2]: (TracingFn.Static[I1, O1], TracingFn.Static[I2, O2]) ?++>
    TracingFn.Static[(I1, I2), (O1, O2)] = { v =>
    ???
  }

  implicit class BinaryForComprehensions[I, O1, O2](
      private val self: TracingFn.Static[I, (O1, O2)]
  ) {
    // TODO: should it be of higher implicit tier?

    // minimal requirement for for-comprehension
    def map[OO](right: ((Input[O1], Input[O2])) => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): TracingFn.Static[I, canChain.Repr] = {
      ???
    }

    def foreach(right: ((Input[O1], Input[O2])) => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): TracingFn.Static[I, Unit] = {
      ???
    }

    def flatMap[I2, OO](right: ((Input[O1], Input[O2])) => TracingFn.Static[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): TracingFn.Static[(I, I2), canChain.Repr] = {
      ???
    }

    def withFilter(right: ((Input[O1], Input[O2])) => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): TracingFn.Static[I, (O1, O2)] = {
      ???
    }
  }

  implicit def tuple2ToOps2[I1, O1, I2, O2]: (TracingFn.Static[I1, O1], TracingFn.Static[I2, O2]) ?++>
    BinaryForComprehensions[(I1, I2), O1, O2] = {
    ???
  }

  implicit class BasicOps[P, I, O](private val self: TracingFn[P, I, O]) {

    // beta reduction, notice that P is contravariant, and Expr[Any, I] represents a static I,
    // so Constructor[Any, I, O] can apply on any Expr[P, I]
    def apply[P2 <: P](arg: Expr.Aux[P2, I])( // TODO: if compiler is strong enough, P2 can be skipped
        implicit
        _definedAt: SrcDefinition
    ): Expr.Aux[P2, O] = {

      ???
    }

    // stolen form ZIO ZLayers, these are shorthands for defining parallel computation graphs
    // they are not necessary but can make definition shorter
    trait zipLike {

      def apply[I2, O2](right: TracingFn[P, I2, O2])(
          implicit
          _definedAt: SrcDefinition
      ): TracingFn[P, (I, I2), (O, O2)]
    }

    object zip extends zipLike {
      override def apply[I2, O2](right: TracingFn[P, I2, O2])(
          implicit
          _definedAt: SrcDefinition
      ): TracingFn[P, (I, I2), (O, O2)] = ???
    }
    def <*> = zip

    //  object zipPar extends zipLike {
    //    override def apply[I2, O2](right: TracingV2[I2, O2])(
    //      implicit
    //      _definedAt: SrcDefinition
    //    ): TracingV2[(I, I2), (O, O2)] = ???
    //  }
    //  def <&> = zipPar

    object union {

      def apply[I2 <: I, O2](right: TracingFn[P, I2, O2])(
          implicit
          _definedAt: SrcDefinition
      ): TracingFn[P, I2, (O, O2)] = {
        ???
      }
    }

    object OrElse {}
    def <> = OrElse

    object OrElseEither {}
    def <+> = OrElseEither
  }
}
