package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.function.hom.Hom
import ai.acyclic.prover.commons.multiverse.rewrite.Delegating
import ai.acyclic.prover.commons.debug.SrcDefinition

/**
  * Representing an elementary function defined by its tracing closure [[base]],
  *
  * TODO: should all kinds of tracing use the same IR (the linear execution record) but different adjoint form?
  *
  * which can be applied on a tracer to yield a [[primaryForm]] (for forward pass) and an [[adjointForm]] (for backward
  * pass),
  *
  * @param base
  *   tracing base function, can be converted to primary & adjoint functions
  * @tparam I
  *   input type
  * @tparam O
  *   output type
  */
case class TracingV2[I, O](
    base: Hom.Fn[Var[I], O]
) extends Delegating[Hom.Fn.K2_[I, O]] {

  @transient lazy val primaryForm: Hom.Fn[I, O] = {
    Hom
      .at[I]
      .apply { i =>
        base(Concrete(i))
      }(base.definedAt)
  }

  @transient lazy val adjointForm: Hom.Fn[I, O] = {
    ???
  }

  def higherOrder(
      implicit
      _definedAt: SrcDefinition
  ): TracingV2[Unit, Hom.Fn[I, O]] =
    TracingV2(
      Hom.at[Var[Unit]] { _ =>
        primaryForm
      }
    )

  def map[O2](right: Var[O] => O2)(
      implicit
      _definedAt: SrcDefinition
  ): TracingV2[I, O2] = {

    val adapted: Hom.Fn[O, O2] = Hom.Fn.Blackbox[O, O2](_definedAt) { o: O =>
      right(Concrete(o))
    }

    val result = Hom.Fn.Mapped[Var[I], O, O2](base, adapted)

    TracingV2(result)
  }

  def flatMap[O2](right: Var[O] => TracingV2[I, O2])(
      implicit
      _definedAt: SrcDefinition
  ): TracingV2[I, O2] = {

    val rightUnboxed: Hom.Fn[Var[I], Hom.Fn[O, O2]] =
      Hom.Fn.Blackbox[Var[I], Hom.Fn[O, O2]](_definedAt) { i: Var[I] =>
        Hom.Fn.Blackbox[O, O2](_definedAt) { o: O =>
          right(Concrete(o)).base(i)
        }
      }

    val result = Hom.Fn.FlatMapped[Var[I], O, O2](base, rightUnboxed)

    TracingV2(result)
  }

  def foreach(right: Var[O] => Unit)(
      implicit
      _definedAt: SrcDefinition
  ): TracingV2[I, Unit] = {

    map(right)
  }

  def withFilter(right: Var[O] => Boolean)(
      implicit
      _definedAt: SrcDefinition
  ): TracingV2[I, O] = {

    val _right = Hom.Fn.Blackbox[O, Boolean](_definedAt) { o: O =>
      right(Concrete(o))
    }

    val result =
      Hom.Fn.Filtered[Var[I], O](base, _right)

    TracingV2(result)
  }

  def ><[I2, O2](right: TracingV2[I2, O2])(
      implicit
      _definedAt: SrcDefinition
  ): TracingV2[(I, I2), (O, O2)] = {

    val pointwiseResult = Hom.Fn.Pointwise(base, right.base)

    // Pointwise returns Fn[(Var[I], Var[I2]), (O, O2)]
    // Need to convert to Fn[Var[(I, I2)], (O, O2)]
    TracingV2(
      Hom.Fn.Blackbox[Var[(I, I2)], (O, O2)](_definedAt) {
        case tupleVar: Var[(I, I2)] =>
          // Extract I and I2 from tupleVar and apply pointwise
          val (vi, vi2) = tupleVar.get
          pointwiseResult((Concrete(vi), Concrete(vi2)))
      }
    )
  }

  def -<[O2](right: TracingV2[I, O2])(
      implicit
      _definedAt: SrcDefinition
  ): TracingV2[I, (O, O2)] = {

    val result: Hom.Fn[Var[I], (O, O2)] =
      Hom.Fn.Blackbox[Var[I], (O, O2)](_definedAt) { i: Var[I] =>
        val iVal = i.get
        (base(i), right.base(i))
      }

    TracingV2(result)
  }

  override protected val unbox: Hom.Fn.K2_[I, O] = primaryForm
}

object TracingV2 {

  type Traceable

  implicit class TracingFnView[I, O](self: Hom.Fn[Var[I], O]) {

//    def tracingRun():
  }

}
