package ai.acyclic.prover.commons.jit.cps

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.hom.Hom.Fn
import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.multiverse.rewrite.Delegating
import ai.acyclic.prover.commons.jit.eval.Args
import Args.{><:, T0}

import scala.language.implicitConversions

/**
  * here are the rules:
  *
  *   - all methods under it becomes extension methods in [[ai.acyclic.prover.commons.jit.tracing.FnImp0]] or its
  *     supertypes
  *   - when creating new instance of [[Continuation]], use existing constructors if possible, ask first before adding
  *     new constructor
  *   - all compilation and tests should succeed
  *   - do not remove or comment out code, specifically code in
  *     [[ai.acyclic.prover.commons.jit.tracing.__TracingDesign]]
  *   - the old class should be removed at last
  *   - your code outside the main package should be minimal
  */
case class Continuation[I <: Args.Prod, +O](
    self: Hom.Fn[I, O]
) extends Delegating[Hom.Fn[I, O]] {

  lazy val higherOrder: Continuation[T0, Hom.Fn[I, O]] = {
    // safe by construction: Const.Provided <: Const.Impl <: Fn[Args.Prod, _] <: Fn[T0, _] (by contravariance)
    Continuation(Hom.Const.Provided(self).asInstanceOf[Hom.Fn[T0, Hom.Fn[I, O]]])
  }

  def map[O2](right: O => O2)(
      implicit
      _definedAt: SrcDefinition
  ): Continuation[I, O2] = {

    val _right: Hom.Fn[O ><: T0, O2] = Hom.Fn.at[O](
      right
    )(_definedAt)

    val result =
      Hom.Fn
        .Mapped[I, O, O2](self, _right)
        .simplify // Mapped[I, O, O2].simplify returns Fn[I, O2]

    Continuation(result)
  }

  def flatMap[O2](right: O => Continuation[I, O2])(
      implicit
      _definedAt: SrcDefinition
  ): Continuation[I, O2] = {

    map(right).flatten
  }

  def foreach(right: O => Unit)(
      implicit
      _definedAt: SrcDefinition
  ): Continuation[I, Unit] = {

    map(right)
  }

  def withFilter(right: O => Boolean)(
      implicit
      _definedAt: SrcDefinition
  ): Continuation[I, O] = {

    val _right = Hom.Fn.Blackbox[O, O](_definedAt) { v =>
      if (right(v)) v // v is already O (Blackbox[O, O] receives O)
      else throw new MatchError(s"condition ${_definedAt} is not applicable on $v")
    }

    val result =
      Hom.Fn.Mapped[I, O, O](self, _right)

    Continuation(result.simplify) // Mapped[I, O, O].simplify returns Fn[I, O]
  }

  object pointwise {

    def apply[I2 <: Args.Prod, O2](
        right: Continuation[I2, O2]
    )(
        implicit
        unzip: Args.Zippable[I, I2]
    ): Continuation[unzip.Zipped, (O, O2)] = {

      // safe by construction: Pointwise constructed with matching types,
      // but compiler can't prove Zippable relationship at this level
      val pointwise = Hom.Fn
        .Pointwise[Any, O, I2, O2, (O, O2)](
          self.asInstanceOf[Hom.Fn[Any ><: T0, O]],
          right.self.asInstanceOf[Hom.Fn[I2, O2]]
        )

      Continuation(pointwise.asInstanceOf[Hom.Fn[unzip.Zipped, (O, O2)]])
    }
  }
  def <*> = pointwise

  object zip {

    def apply[I2 <: I, O2](right: Continuation[I2, O2]): Continuation[I2, (O, O2)] = {

      val first: Hom.Fn.DuplicateArgs[I2] = Hom.Fn.DuplicateArgs[I2]()
      // safe by construction: Pointwise matches structurally, but compiler can't prove the type relationships
      val second: Hom.Fn.Pointwise[Any, O, I2, O2, (O, O2)] =
        Hom.Fn.Pointwise(self.asInstanceOf[Hom.Fn[Any ><: T0, O]], right.self.asInstanceOf[Hom.Fn[I2, O2]])

      val result =
        Hom.Fn.Mapped[I2, (I2, I2), (O, O2)](first, second.asInstanceOf[Hom.Fn[(I2, I2) ><: T0, (O, O2)]])

      Continuation(result.asInstanceOf[Hom.Fn[I2, (O, O2)]])
    }
  }
  def -< = zip

  // flatMap is undefined, there are several options, see dottyspike ForComprehension spike for details

  override lazy val unbox: Hom.Fn[I, O] = self.simplify
}

object Continuation {
  // Implicit conversions are provided by Delegating.unbox1

  // Additional implicit conversion from Tracing to Function1View for function composition
  implicit def tracingToFunction[I <: Args.Prod, O](v: Continuation[I, O])(
      implicit
      _definedAt: SrcDefinition
  ): Function1[I, O] = {
    new Function1[I, O] {
      override def apply(arg: I): O = v.unbox(arg)
    }
  }

  implicit class CanFlatten[I <: Args.Prod, O](continuation: Continuation[I, Continuation[I, O]]) {

    def flatten(
        implicit
        _definedAt: SrcDefinition // TODO: this should not be required
    ): Continuation[I, O] = {

      val prev: Fn[I, Continuation[I, O]] = continuation.self.asInstanceOf[Hom.Fn[I, Continuation[I, O]]]

      val result =
        Hom.Fn.Flatten[I, Continuation[I, O], O](
          // safe by construction: continuation wraps Fn[I, Continuation[I, O]], coerce extracts inner Fn
          prev,
          { v: Continuation[I, O] => v.self.asInstanceOf[Hom.Fn[I, O]] }
        )

      Continuation(result.simplify) // Flatten[I,_,O].simplify returns Fn[I, O]
    }
  }
}
