package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.multiverse.rewrite.HasConversionPart

import scala.language.implicitConversions

trait FnImp0 extends FnImp1 with HasConversionPart {
  self: Expr.type =>

  implicit def _forInput_<-[I, O]: Conversion[TracingFn[I, O], ForInputComprehensions[I, O]] =
    new Conversion[TracingFn[I, O], ForInputComprehensions[I, O]] {
      override def normalise(v: TracingFn[I, O]): ForInputComprehensions[I, O] =
        ForInputComprehensions(v)
    }

  case class ForInputComprehensions[I, O](
      self: TracingFn[I, O]
  ) extends PartiallyConverted {

    // minimal requirement for for-comprehension
    def mapExpr[OO](right: Input[O] => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): TracingFn[I, canChain.Repr] = {

      val rightFn = Hom.Fn.at[O] { o =>
        canChain.parse(right(Const(o))).reify(_definedAt)
      }(_definedAt)

      val result = Hom.Fn.Mapped(self.concrete, rightFn)
      TracingFn(result)
    }

    def map[O2](right: O => O2)(
        implicit
        _definedAt: SrcDefinition
    ): TracingFn[I, O2] = {

      val rightFn = Hom.Fn.at[O](right)(_definedAt)
      val result = Hom.Fn.Mapped(self.concrete, rightFn)
      TracingFn(result)
    }

    def foreachExpr(right: Input[O] => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): TracingFn[I, Unit] = {

      val rightFn = Hom.Fn.at[O] { o =>
        right(Const(o))
      }(_definedAt)

      val result = Hom.Fn.Mapped(self.concrete, rightFn)
      TracingFn(result)
    }

    def foreach(right: O => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): TracingFn[I, Unit] = {

      val rightFn = Hom.Fn.at[O](right)(_definedAt)
      val result = Hom.Fn.Mapped(self.concrete, rightFn)
      TracingFn(result)
    }

    def flatMapExpr[I2, OO](right: Input[O] => TracingFn[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): TracingFn[(I, I2), canChain.Repr] = {

      val proto: Hom.:=>[Input[(I, I2)], Expr[canChain.Repr]] = Hom.:=>.at[Input[(I, I2)]] { input =>
        val (i, i2) = input.reify
        val o: O = self.concrete(i)
        val nextFn = right(Const(o))
        val oo = nextFn.concrete(i2)
        canChain.parse(oo)
      }(_definedAt)

      TracingFn.Unary(proto)
    }

    def flatMap[I2, OO](right: O => TracingFn[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): TracingFn[(I, I2), canChain.Repr] = {

      val proto: Hom.:=>[Input[(I, I2)], Expr[canChain.Repr]] = Hom.:=>.at[Input[(I, I2)]] { input =>
        val (i, i2) = input.reify
        val o: O = self.concrete(i)
        val nextFn = right(o)
        val oo = nextFn.concrete(i2)
        canChain.parse(oo)
      }(_definedAt)

      TracingFn.Unary(proto)
    }

    def withFilterExpr(right: Input[O] => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): TracingFn[I, O] = {

      val rightFn = Hom.Fn.at[O] { o =>
        right(Const(o))
      }(_definedAt)

      val result = Hom.Fn.Filtered(self.concrete, rightFn)
      TracingFn(result)
    }

    def withFilter(right: O => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): TracingFn[I, O] = {

      val rightFn = Hom.Fn.at[O](right)(_definedAt)
      val result = Hom.Fn.Filtered(self.concrete, rightFn)
      TracingFn(result)
    }
  }

  implicit def tuple2ToFn[I1, O1, I2, O2]: (TracingFn[I1, O1], TracingFn[I2, O2]) ?++>
    TracingFn[(I1, I2), (O1, O2)] = { tuple =>
    val (f1, f2) = tuple

    val result = Hom.Fn.Pointwise(f1.concrete, f2.concrete)

    TracingFn(result)
  }

}
