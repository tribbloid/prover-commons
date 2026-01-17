package ai.acyclic.prover.commons.jit.tracingV1

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.multiverse.rewrite.Delegating
import ai.acyclic.prover.commons.TypeTag

import scala.language.implicitConversions

case class TracingFn[I, +O](
    concrete: Hom.Fn[I, O]
) extends Delegating[Hom.Fn[I, O]] {

  lazy val higherOrder: TracingFn[Unit, Hom.Fn[I, O]] = {
    TracingFn(Hom.Thunk.Static(concrete))
  }

  def map[O2](right: O => O2)(
      implicit
      _definedAt: SrcDefinition,
      o2Tag: TypeTag[O2]
  ): TracingFn[I, O2] = {

    val _right: Hom.Fn[O, O2] = Hom.Fn.at[O](
      right
    )(_definedAt)

    val result =
      Hom.Fn.Mapped[I, O, O2](concrete, _right)

    TracingFn(result)
  }

  def flatMap[I2, O2](right: O => TracingFn[I2, O2])(
      implicit
      _definedAt: SrcDefinition
  ): TracingFn[(I, I2), O2] = {

    val _ = right

    ???
  }

  def foreach(right: O => Unit)(
      implicit
      _definedAt: SrcDefinition
  ): TracingFn[I, Unit] = {

    map(right)
  }

  def withFilter(right: O => Boolean)(
      implicit
      _definedAt: SrcDefinition
  ): TracingFn[I, O] = {

    val _right = Hom.Fn.Blackbox[O, Boolean](_definedAt)(right)

    val result =
      Hom.Fn.Filtered[I, O](concrete, _right)

    TracingFn(result)
  }

  def <*>[I2, O2](right: TracingFn[I2, O2]): TracingFn[(I, I2), (O, O2)] = {

    val result = Hom.Fn.Pointwise(concrete, right.concrete)

    TracingFn(result.normalForm)
  }

  def -<[O2](right: TracingFn[I, O2]): TracingFn[I, (O, O2)] = {

    val first = Hom.Fn.Duplicate[I]()
    val second = Hom.Fn.Pointwise(concrete, right.concrete)

    val result = Hom.Fn.Mapped[I, (I, I), (O, O2)](first, second)

    TracingFn(result)
  }

  // flatMap is undefined, there are several options, see dottyspike ForComprehension spike for details

  override lazy val unbox: Hom.Fn[I, O] = concrete.normalForm
}

object TracingFn {
  // Implicit conversions are provided by Delegating.unbox1

  // Additional implicit conversion from Tracing to Function1View for function composition
  implicit def tracingToFunction[I, O](v: TracingFn[I, O])(
      implicit
      _definedAt: SrcDefinition
  ): Hom.HasNormalForm.Function1View[I, O] = {
    Hom.HasNormalForm._as1View(v.unbox)
  }
}
