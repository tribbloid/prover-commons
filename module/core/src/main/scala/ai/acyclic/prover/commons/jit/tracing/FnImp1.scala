package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.jit.hom.Hom.:=>
import ai.acyclic.prover.commons.multiverse.rewrite.HasConversionPart

trait FnImp1 extends FnCanChain with HasConversionPart {

  implicit class UnaryForComprehensions[I, O](
      private val self: Expr.Static[Hom.Fn[I, O]]
  ) {

    // minimal requirement for for-comprehension
    def mapExpr[OO](right: Input[O] => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[I, canChain.Repr]] = {

      val rightFn = Hom.Fn.at[O] { o =>
        canChain.parse(right(Const(o))).getConcrete(_definedAt)
      }(_definedAt)

      val result = Hom.Fn.Mapped(self.concrete, rightFn)
      TracingFn(result)
    }

    def map[O2](right: O => O2)(
        implicit
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[I, O2]] = {

      val rightFn = Hom.Fn.at[O](right)(_definedAt)
      val result = Hom.Fn.Mapped(self.concrete, rightFn)
      TracingFn(result)
    }

    def foreachExpr(right: Input[O] => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[I, Unit]] = {

      val rightFn = Hom.Fn.at[O] { o =>
        right(Const(o))
      }(_definedAt)

      val result = Hom.Fn.Mapped(self.concrete, rightFn)
      TracingFn(result)
    }

    def foreach(right: O => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[I, Unit]] = {

      val rightFn = Hom.Fn.at[O](right)(_definedAt)
      val result = Hom.Fn.Mapped(self.concrete, rightFn)
      TracingFn(result)
    }

    def flatMapExpr[I2, OO](right: Input[O] => Expr.Static[Hom.Fn[I2, OO]])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[(I, I2), canChain.Repr]] = {

      val proto: Hom.:=>[Input[(I, I2)], Expr[canChain.Repr]] = Hom.:=>.at[Input[(I, I2)]] { input =>
        val (i, i2) = input.getConcrete
        val o: O = self.concrete(i)
        val nextFn = right(Const(o))
        val oo = nextFn.concrete(i2)
        canChain.parse(oo)
      }(_definedAt)

      TracingFn.Impl(proto)
    }

    def flatMap[I2, OO](right: O => Expr.Static[Hom.Fn[I2, OO]])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[(I, I2), canChain.Repr]] = {

      val proto: Hom.:=>[Input[(I, I2)], Expr[canChain.Repr]] = Hom.:=>.at[Input[(I, I2)]] { input =>
        val (i, i2) = input.getConcrete
        val o: O = self.concrete(i)
        val nextFn = right(o)
        val oo = nextFn.concrete(i2)
        canChain.parse(oo)
      }(_definedAt)

      TracingFn.Impl(proto)
    }

    def withFilterExpr(right: Input[O] => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[I, O]] = {

      val rightFn = Hom.Fn.at[O] { o =>
        right(Const(o))
      }(_definedAt)

      val result = Hom.Fn.Filtered(self.concrete, rightFn)
      TracingFn(result)
    }

    def withFilter(right: O => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): Expr.Static[Hom.Fn[I, O]] = {

      val rightFn = Hom.Fn.at[O](right)(_definedAt)
      val result = Hom.Fn.Filtered(self.concrete, rightFn)
      TracingFn(result)
    }
  }

  // TODO: the following can be removed by carefully using ConversionPart
  implicit def tuple2ToOps1[I1, O1, I2, O2]: (TracingFn.Static[I1, O1], TracingFn.Static[I2, O2]) ?++>
    UnaryForComprehensions[(I1, I2), (O1, O2)] = {
    ???
  }
}
