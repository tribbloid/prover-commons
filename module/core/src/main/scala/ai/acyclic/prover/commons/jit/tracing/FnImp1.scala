package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.multiverse.rewrite.HasConversionPart

import scala.language.implicitConversions

//trait FnImp0 extends HasConversionPart with ExprPriority1 {
trait FnImp1 extends HasConversionPart {
  self: Expr.type =>

  // Additional implicit conversion from Tracing to Function1View for function composition
  implicit def tracingToFunctionView[I, O](v: TracingFn[I, O])(
      implicit
      _definedAt: SrcDefinition
  ): Hom.HasNormalForm.Function1View[I, O] = {
    Hom.HasNormalForm._as1View(v.concrete)
  }

//  implicit def

  /**
    * one implicit class the rules all for-comprehension:
    *
    *   - can handle for comprehension with unary input
    *   - can hanlde for comprehension with input of tuple of arbitrary sizes
    *   - use [[CanReifyMany]] to convert a single [[Input]] tuple of [[Input]] into tuple of values
    *   - yields [[TracingFn.Impl]] [[TracingFn.Unary]]
    *
    * unfortunately Scala compiler is too weak to deduce function argument type from lambda, otherwise
    * [[ai.acyclic.prover.commons.jit.tracing.Expr.ForInputComprehensions]] can be removed
    */
  implicit def _forTuple_<-[IInputs, O](
      self: TracingFn[IInputs, O]
  ): ForTupleComprehensions[IInputs, O] = ForTupleComprehensions(self)

  case class ForTupleComprehensions[IInputs, O](
      self: TracingFn[IInputs, O]
  ) extends PartiallyConverted {

    // minimal requirement for for-comprehension
    def map[OO, ITuple](right: ITuple => OO)(
        implicit
        canReify: CanReifyMany.Aux[O, ITuple],
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): TracingFn[IInputs, canChain.Repr] = {

      val rightFn = Hom.Fn.at[O] { o =>
        val v = canReify.reify(o)
        canChain.parse(right(v)).reify(_definedAt)
      }(_definedAt)

      val result = Hom.Fn.Mapped(self.concrete, rightFn)
      TracingFn(result)
    }

    def foreach[Unpacked](right: Unpacked => Unit)(
        implicit
        canReify: CanReifyMany.Aux[O, Unpacked],
        _definedAt: SrcDefinition
    ): TracingFn[IInputs, Unit] = {
      map(right)
    }

    def flatMap[I2, OO, Unpacked](right: Unpacked => TracingFn[I2, OO])(
        implicit
        canReify: CanReifyMany.Aux[O, Unpacked],
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): TracingFn[(IInputs, I2), canChain.Repr] = {

      val proto: Hom.:=>[Input[(IInputs, I2)], Expr[canChain.Repr]] = Hom.:=>.at[Input[(IInputs, I2)]] { input =>
        val (i, i2) = input.reify
        val o = self.concrete(i)

        val v = canReify.reify(o)
        val nextFn = right(v)

        val oo = nextFn.concrete(i2)
        canChain.parse(oo)
      }(_definedAt)

      TracingFn.Unary(proto)
    }

    def withFilter[Unpacked](right: Unpacked => Boolean)(
        implicit
        canReify: CanReifyMany.Aux[O, Unpacked],
        _definedAt: SrcDefinition
    ): TracingFn[IInputs, O] = {

      val rightFn = Hom.Fn.at[O] { o =>
        val v = canReify.reify(o)
        right(v)
      }(_definedAt)

      val result = Hom.Fn.Filtered(self.concrete, rightFn)
      TracingFn(result)
    }
  }

  implicit class BasicOps[P, I, O](private val self: TracingFnLike[P, I, O]) {

    // beta reduction, notice that P is contravariant, and Expr[Any, I] represents a static I,
    // so Constructor[Any, I, O] can apply on any Expr[P, I]
    // beta reduction, notice that P is contravariant, and Expr[Any, I] represents a static I,
    // so Constructor[Any, I, O] can apply on any Expr[P, I]
    def apply[P2 <: P](arg: Expr.Aux[P2, I])( // TODO: if compiler is strong enough, P2 can be skipped
        implicit
        _definedAt: SrcDefinition
    ): Expr.Static[O] = {

      val v: I = arg.reify
      val result: O = self.reify(_definedAt)(v)
      Const(result)
    }

    // stolen form ZIO ZLayers, these are shorthands for defining parallel computation graphs
    // they are not necessary but can make definition shorter
    trait zipLike {

      def apply[I2, O2](right: TracingFnLike[P, I2, O2])(
          implicit
          _definedAt: SrcDefinition
      ): TracingFn[(I, I2), (O, O2)]
    }

    object zip extends zipLike {
      override def apply[I2, O2](right: TracingFnLike[P, I2, O2])(
          implicit
          _definedAt: SrcDefinition
      ): TracingFn[(I, I2), (O, O2)] = {

        val result = Hom.Fn.Pointwise(self.reify, right.reify)
        TracingFn(result) // returns Static which is subtype of Expr[P, ...]
      }
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

      def apply[I2 <: I, O2](right: TracingFnLike[P, I2, O2])(
          implicit
          _definedAt: SrcDefinition
      ): TracingFn[I2, (O, O2)] = {

        val duplicate = Hom.Fn.Duplicate[I2]()
        val pointwise = Hom.Fn.Pointwise(self.reify, right.reify)

        val result = Hom.Fn.Mapped(duplicate, pointwise)
        TracingFn(result)
      }
    }

    object OrElse {}
    def <> = OrElse

    object OrElseEither {}
    def <+> = OrElseEither
  }
}
