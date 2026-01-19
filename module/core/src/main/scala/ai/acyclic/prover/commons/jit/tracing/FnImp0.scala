package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.multiverse.rewrite.HasConversionPart

import scala.language.implicitConversions

trait FnImp0 extends HasConversionPart with ExprPriority1 {

  // Additional implicit conversion from Tracing to Function1View for function composition
  implicit def tracingToFunction[I, O](v: Expr.Static[Hom.Fn[I, O]])(
      implicit
      _definedAt: SrcDefinition
  ): Hom.HasNormalForm.Function1View[I, O] = {
    Hom.HasNormalForm._as1View(v.concrete)
  }

  implicit def tuple2ToFn[I1, O1, I2, O2]: (Expr.Static[Hom.Fn[I1, O1]], Expr.Static[Hom.Fn[I2, O2]]) ?++>
    Expr.Static[Hom.Fn[(I1, I2), (O1, O2)]] = { tuple =>
    val (f1, f2) = tuple

    val result = Hom.Fn.Pointwise(f1.concrete, f2.concrete)

    TracingFn(result)
  }

  /**
    * TODO: this class and [[UnaryForComprehensions]] should be merged into a single implicit class that:
    *
    *   - can handle for comprehension with unary input
    *   - can hanlde for comprehension with input of tuple of arbitrary sizes
    *   - use [[CanReifyMany]] to convert tuple of [[Input]] into tuple of values
    *   - output should use [[TracingFn.Impl]] constructor or [[TracingFn.Unary]]
    *   - all tests should pass
    */

  implicit class ForComprehensions[Inputs, O](
      private val self: Expr.Static[Hom.Fn[Inputs, O]]
  ) {

    // minimal requirement for for-comprehension
    def map[OO, Unpacked](right: Unpacked => OO)(
        implicit
        canReify: CanReifyMany.Aux[O, Unpacked],
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[Inputs, canChain.Repr]] = {

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
    ): Expr.Static[Hom.Fn[Inputs, Unit]] = {
      map(right).asInstanceOf[Expr.Static[Hom.Fn[Inputs, Unit]]]
    }

    def flatMap[I2, OO, Unpacked](right: Unpacked => Expr.Static[Hom.Fn[I2, OO]])(
        implicit
        canReify: CanReifyMany.Aux[O, Unpacked],
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[(Inputs, I2), canChain.Repr]] = {

      val proto: Hom.:=>[Input[(Inputs, I2)], Expr[canChain.Repr]] = Hom.:=>.at[Input[(Inputs, I2)]] { input =>
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
    ): Expr.Static[Hom.Fn[Inputs, O]] = {

      val rightFn = Hom.Fn.at[O] { o =>
        val v = canReify.reify(o)
        right(v)
      }(_definedAt)

      val result = Hom.Fn.Filtered(self.concrete, rightFn)
      TracingFn(result)
    }
  }

  implicit class BasicOps[P, I, O](private val self: Expr.Gt[P, Hom.Fn[I, O]]) {

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

      def apply[I2, O2](right: Expr.Gt[P, Hom.Fn[I2, O2]])(
          implicit
          _definedAt: SrcDefinition
      ): Expr.Static[Hom.Fn[(I, I2), (O, O2)]]
    }

    object zip extends zipLike {
      override def apply[I2, O2](right: Expr.Gt[P, Hom.Fn[I2, O2]])(
          implicit
          _definedAt: SrcDefinition
      ): Expr.Static[Hom.Fn[(I, I2), (O, O2)]] = {

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

      def apply[I2 <: I, O2](right: Expr.Gt[P, Hom.Fn[I2, O2]])(
          implicit
          _definedAt: SrcDefinition
      ): Expr.Static[Hom.Fn[I2, (O, O2)]] = {

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
