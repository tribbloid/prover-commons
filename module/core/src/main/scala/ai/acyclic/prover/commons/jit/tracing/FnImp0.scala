package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.hom.Hom

import scala.language.implicitConversions

trait FnImp0 extends FnImp1 with ExprPriority1 {

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

  implicit class BinaryForComprehensions[I, O1, O2](
      private val self: Expr.Static[Hom.Fn[I, (O1, O2)]]
  ) {

    // minimal requirement for for-comprehension
    def map[OO](right: ((Input[O1], Input[O2])) => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[I, canChain.Repr]] = {

      val rightFn = Hom.Fn.at[(O1, O2)] {
        case (o1, o2) =>
          canChain.parse(right((Const(o1), Const(o2)))).reify(_definedAt)
      }(_definedAt)

      val result = Hom.Fn.Mapped(self.concrete, rightFn)
      TracingFn(result)
    }

    def foreach(right: ((Input[O1], Input[O2])) => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[I, Unit]] = {
      map(right)
    }

    def flatMap[I2, OO](right: ((Input[O1], Input[O2])) => Expr.Static[Hom.Fn[I2, OO]])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[(I, I2), canChain.Repr]] = {

      val proto: Hom.:=>[Input[(I, I2)], Expr[canChain.Repr]] = Hom.:=>.at[Input[(I, I2)]] { input =>
        val (i, i2) = input.reify
        val (o1, o2) = self.concrete(i)
        val nextFn = right((Const(o1), Const(o2)))
        val oo = nextFn.concrete(i2)
        canChain.parse(oo)
      }(_definedAt)

      TracingFn.Unary(proto)
    }

    def withFilter(right: ((Input[O1], Input[O2])) => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[I, (O1, O2)]] = {

      val rightFn = Hom.Fn.at[(O1, O2)] {
        case (o1, o2) =>
          right((Const(o1), Const(o2)))
      }(_definedAt)

      val result = Hom.Fn.Filtered(self.concrete, rightFn)
      TracingFn(result)
    }
  }

  implicit def tuple2ToOps2[I1, O1, I2, O2]: (Expr.Static[Hom.Fn[I1, O1]], Expr.Static[Hom.Fn[I2, O2]]) ?++>
    BinaryForComprehensions[(I1, I2), O1, O2] = { pair =>
    val combined = tuple2ToFn(pair)
    new BinaryForComprehensions(combined)
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
