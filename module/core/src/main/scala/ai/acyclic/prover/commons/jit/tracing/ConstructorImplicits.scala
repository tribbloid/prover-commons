package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

trait ConstructorImplicits extends ConstructorCanChain {

  implicit class UnaryForComprehensionOps[I, O](private val self: TracingFn[I, O]) {

    // minimal requirement for for-comprehension
    def map[OO](right: Var[O] => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): TracingFn[I, canChain.Repr] = {
      ???
    }

    def foreach(right: Var[O] => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): TracingFn[I, Unit] = {
      ???
    }

    def flatMap[I2, OO](right: Var[O] => TracingFn[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): TracingFn[(I, I2), canChain.Repr] = {
      ???
    }

    def withFilter(right: Var[O] => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): TracingFn[I, O] = {

      ???
    }
  }

  implicit class BinaryForComprehensionOps[I, O1, O2](private val self: TracingFn[I, (O1, O2)]) {
    // TODO: should it be of higher implicit tier?

    // minimal requirement for for-comprehension
    def map[OO](right: ((Var[O1], Var[O2])) => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): TracingFn[I, canChain.Repr] = {
      ???
    }

    def foreach(right: ((Var[O1], Var[O2])) => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): TracingFn[I, Unit] = {
      ???
    }

    def flatMap[I2, OO](right: ((Var[O1], Var[O2])) => TracingFn[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): TracingFn[(I, I2), canChain.Repr] = {
      ???
    }

    def withFilter(right: ((Var[O1], Var[O2])) => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): TracingFn[I, (O1, O2)] = {
      ???
    }
  }

  implicit class BasicOps[P, I, O](private val self: ConstructorLike[P, I, O]) {

    // beta reduction, notice that P is contravariant, and Expr[Any, I] represents a static I,
    // so Constructor[Any, I, O] can apply on any Expr[P, I]
    def apply(arg: Expr.Gt[P, I])(
        implicit
        _definedAt: SrcDefinition
    ): Expr._1[P, O] = {

      ???
    }

    // enable currying, calculus of variations
//  def liftToHigherOrder( // TODO: remove, this should happen automatically
//      implicit
//      _definedAt: SrcDefinition
//  ): Constructor[Unit, I :=> O] = {
//    ???
//  }

    // stolen form ZIO ZLayers, these are shorthands for defining parallel computation graphs
    // they are not necessary but can make definition shorter
    trait zipLike {

      def apply[I2, O2](right: ConstructorLike[P, I2, O2])(
          implicit
          _definedAt: SrcDefinition
      ): ConstructorLike[P, (I, I2), (O, O2)]
    }

    object zip extends zipLike {
      override def apply[I2, O2](right: ConstructorLike[P, I2, O2])(
          implicit
          _definedAt: SrcDefinition
      ): ConstructorLike[P, (I, I2), (O, O2)] = ???
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

      def apply[I2 <: I, O2](right: ConstructorLike[P, I2, O2])(
          implicit
          _definedAt: SrcDefinition
      ): ConstructorLike[P, I2, (O, O2)] = {
        ???
      }
    }

    object OrElse {}
    def <> = OrElse

    object OrElseEither {}
    def <+> = OrElseEither
  }
}
