package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.hom.Hom.:=>

trait FnImplicits extends FnCanChain {

  implicit class UnaryForComprehensions[I, O](private val self: StaticTracingFn[I, O]) {

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

  implicit class BinaryForComprehensions[I, O1, O2](private val self: StaticTracingFn[I, (O1, O2)]) {
    // TODO: should it be of higher implicit tier?

    // minimal requirement for for-comprehension
    def map[OO](right: ((Input[O1], Input[O2])) => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): StaticTracingFn[I, canChain.Repr] = {
      ???
    }

    def foreach(right: ((Input[O1], Input[O2])) => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): StaticTracingFn[I, Unit] = {
      ???
    }

    def flatMap[I2, OO](right: ((Input[O1], Input[O2])) => StaticTracingFn[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): StaticTracingFn[(I, I2), canChain.Repr] = {
      ???
    }

    def withFilter(right: ((Input[O1], Input[O2])) => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): StaticTracingFn[I, (O1, O2)] = {
      ???
    }
  }

  implicit class Tuple2Ops[I1, I2](private val self: (StaticTracingFn[I1, I1], StaticTracingFn[I2, I2])) {

    def map[OO](right: ((Input[I1], Input[I2])) => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): StaticTracingFn[(I1, I2), canChain.Repr] = {
      ???
    }

    def foreach(right: ((Input[I1], Input[I2])) => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): StaticTracingFn[(I1, I2), Unit] = {
      ???
    }

    def flatMap[I3, OO](right: ((Input[I1], Input[I2])) => StaticTracingFn[I3, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): StaticTracingFn[(I1, I2, I3), canChain.Repr] = {
      ???
    }

    def withFilter(right: ((Input[I1], Input[I2])) => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): (Expr[I1 :=> I1], Expr[I2 :=> I2]) = {

      ???
    }
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
