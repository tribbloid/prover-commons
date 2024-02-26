package ai.acyclic.prover.commons.jit.cps

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.hom.Hom.{Fn, Function1View}
import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.multiverse.rewrite.Delegating
import zio.Unzippable

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
case class Continuation[I, +O](
    self: Hom.Fn[I, O]
) extends Delegating[Hom.Fn[I, O]] {

  lazy val higherOrder: Continuation[Unit, Hom.Fn[I, O]] = {
    Continuation(Hom.Const.Provided(self))
  }

  def map[O2](right: O => O2)(
      implicit
      _definedAt: SrcDefinition
  ): Continuation[I, O2] = {

    val _right: Hom.Fn[O, O2] = Hom.Fn.at[O](
      right
    )(_definedAt)

    val result =
      Hom.Fn.Mapped[I, O, O2](self, _right).simplify

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
      if (right(v)) v
      else throw new MatchError(s"condition ${_definedAt} is not applicable on $v")
    }

    val result =
      Hom.Fn.Mapped[I, O, O](self, _right)

    Continuation(result)
  }

  object pointwise {

    def apply[I2, O2](
        right: Continuation[I2, O2]
    )(
        implicit
        unzip: Unzippable[I, I2]
    ): Continuation[unzip.In, (O, O2)] = {

      val unzipper: Hom.Fn[unzip.In, (I, I2)] = Hom.Fn.Blackbox(
        SrcDefinition.Unknown(java.util.UUID.randomUUID())
      )(unzip.unzip)
      val pointwise = Hom.Fn.Pointwise(self, right.self)
      val result = Hom.Fn.Mapped(unzipper, pointwise)

      Continuation(result.simplify)
    }
  }
  def <*> = pointwise

  object zip {

    def apply[I2 <: I, O2](right: Continuation[I2, O2]): Continuation[I2, (O, O2)] = {

      val first: Hom.Fn.Duplicate[I2] = Hom.Fn.Duplicate[I2]()
      val second: Hom.Fn.Pointwise[I2, O, I2, O2] = Hom.Fn.Pointwise(self: Hom.Fn[I2, O], right.self)

      val result = Hom.Fn.Mapped(first, second)

      Continuation(result)
    }
  }
  def -< = zip

  // flatMap is undefined, there are several options, see dottyspike ForComprehension spike for details

  override lazy val unbox: Hom.Fn[I, O] = self.simplify
}

object Continuation {
  // Implicit conversions are provided by Delegating.unbox1

  // Additional implicit conversion from Tracing to Function1View for function composition
  implicit def tracingToFunction[I, O](v: Continuation[I, O])(
      implicit
      _definedAt: SrcDefinition
  ): Function1View[I, O] = {
    Fn._as1View(v.unbox)
  }

  implicit class CanFlatten[I, O](continuation: Continuation[I, Continuation[I, O]]) {

    def flatten(
        implicit
        _definedAt: SrcDefinition // TODO: this should not be required
    ): Continuation[I, O] = {

      val result =
        Hom.Fn.Flatten(continuation.self, { v: Continuation[I, O] => v.self })

      Continuation(result.simplify)
    }
  }
}
