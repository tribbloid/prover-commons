package ai.acyclic.prover.commons.function.tracingV2

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.hom.Hom
import ai.acyclic.prover.commons.multiverse.rewrite.Delegating

import scala.language.implicitConversions

case class Tracing[I, O](self: Hom.Fn[I, O]) extends Delegating[Hom.Fn.K2_[I, O]] {

  lazy val higherOrder: Tracing[Unit, Hom.Fn[I, O]] =
    Tracing(Hom.Thunk.CachedEager(self))

//  def apply(v: I): O = self(v)

  def map[O2](right: O => O2)(
      implicit
      _definedAt: SrcDefinition
  ): Tracing[I, O2] = {

    val _right: Hom.Fn[O, O2] = Hom.Fn.at[O](right)(_definedAt)

    val result =
      Hom.Fn.Mapped[I, O, O2](self, _right)

    Tracing(result)
  }

  def flatMap[I2, O2](right: O => Tracing[I2, O2])(
      implicit
      _definedAt: SrcDefinition
  ): Tracing[(I, I2), O2] = {

    right

    ???
  }

  def foreach(right: O => Unit)(
      implicit
      _definedAt: SrcDefinition
  ): Tracing[I, Unit] = {

    map(right)
  }

  def withFilter(right: O => Boolean)(
      implicit
      _definedAt: SrcDefinition
  ): Tracing[I, O] = {

    val _right = Hom.Fn.Blackbox[O, Boolean](_definedAt)(right)

    val result =
      Hom.Fn.Filtered[I, O](self, _right)

    Tracing(result)
  }

  def ><[I2, O2](right: Tracing[I2, O2]): Tracing[(I, I2), (O, O2)] = {

    val result = Hom.Fn.Pointwise(self, right.self)

    Tracing(result.normalForm)
  }

  def -<[O2](right: Tracing[I, O2]): Tracing[I, (O, O2)] = {

    val first = Hom.Fn.Duplicate[I]()
    val second = Hom.Fn.Pointwise(self, right.self)

    val result = Hom.Fn.Mapped[I, (I, I), (O, O2)](first, second)

    Tracing(result)
  }

  // flatMap is undefined, there are several options, see dottyspike ForComprehension spike for details

  override lazy val unbox: Hom.Fn[I, O] = self.normalForm
}

object Tracing {
  // Implicit conversions are provided by Delegating.unbox1

  // Additional implicit conversion from Tracing to Function1View for function composition
  implicit def tracingToFunction[I, O](v: Tracing[I, O])(
      implicit
      _definedAt: SrcDefinition
  ): Hom.HasNormalForm.Function1View[I, O] = {
    Hom.HasNormalForm._as1View(v.unbox)
  }
}
