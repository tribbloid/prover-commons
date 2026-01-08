//package ai.acyclic.prover.commons.function.tracing
//
//import ai.acyclic.prover.commons.function.hom.Hom
//import ai.acyclic.prover.commons.multiverse.rewrite.Delegating
//import ai.acyclic.prover.commons.debug.SrcDefinition
//
///**
//  * Representing an elementary function defined by its tracing closure [[base]],
//  *
//  * TODO: should all kinds of tracing use the same IR (the linear execution record) but different adjoint form? - yes
//  *
//  * TODO: when planning for fusing expensive operations, how to prompt traced expensive operations to produce only a
//  * placeholder instead of actual value? (remember that inputs are already concrete, but operations should not be
//  * evaluated), unfortunately, the API only accepts a closure from FusedRow to new columns. As a result, the variable
//  * can be very different from that in for-comprehension.
//  *
//  * In the end, all functions (like Wget, Interpret or Prompt) should have an adjoint function, executing it results in
//  * an [[Expression]] instead of a value (if it just return Unit there is no difference), the problem becomes that each
//  * of them interact with the Env, so their output [[Expression]] should also include the Env.
//  *
//  * if this can be fully captured by a tape/expr, so should be other things
//  *
//  * which can be applied on a tracer to yield a [[primaryForm]] (for forward execution) and
//  *
//  * @param base
//  *   tracing base function, can be converted to primary & adjoint functions
//  * @tparam I
//  *   input type
//  * @tparam O
//  *   output type
//  */
//case class TracingV2[I, O](
//    base: Hom.Fn[Tracer[I], O]
//) extends Delegating[Hom.Fn.K2_[I, O]] {
//
//  @transient lazy val primaryForm: Hom.Fn[I, O] = {
//    Hom
//      .at[I]
//      .apply { i =>
//        base(Concrete(i))
//      }(base.definedAt)
//  }
//
//  def higherOrder(
//      implicit
//      _definedAt: SrcDefinition
//  ): TracingV2[Unit, Hom.Fn[I, O]] =
//    TracingV2(
//      Hom.at[Tracer[Unit]] { _ =>
//        primaryForm
//      }
//    )
//
//  def map[O2](right: Tracer[O] => O2)(
//      implicit
//      _definedAt: SrcDefinition
//  ): TracingV2[I, O2] = {
//
//    val adapted: Hom.Fn[O, O2] = Hom.Fn.Blackbox[O, O2](_definedAt) { o: O =>
//      right(Concrete(o))
//    }
//
//    val result = Hom.Fn.Mapped[Tracer[I], O, O2](base, adapted)
//
//    TracingV2(result)
//  }
//
//  def flatMap[O2](right: Tracer[O] => TracingV2[I, O2])(
//      implicit
//      _definedAt: SrcDefinition
//  ): TracingV2[I, O2] = {
//
//    val rightUnboxed: Hom.Fn[Tracer[I], Hom.Fn[O, O2]] =
//      Hom.Fn.Blackbox[Tracer[I], Hom.Fn[O, O2]](_definedAt) { i: Tracer[I] =>
//        Hom.Fn.Blackbox[O, O2](_definedAt) { o: O =>
//          right(Concrete(o)).base(i)
//        }
//      }
//
//    val result = Hom.Fn.FlatMapped[Tracer[I], O, O2](base, rightUnboxed)
//
//    TracingV2(result)
//  }
//
//  def foreach(right: Tracer[O] => Unit)(
//      implicit
//      _definedAt: SrcDefinition
//  ): TracingV2[I, Unit] = {
//
//    map(right)
//  }
//
//  def withFilter(right: Tracer[O] => Boolean)(
//      implicit
//      _definedAt: SrcDefinition
//  ): TracingV2[I, O] = {
//
//    val _right = Hom.Fn.Blackbox[O, Boolean](_definedAt) { o: O =>
//      right(Concrete(o))
//    }
//
//    val result =
//      Hom.Fn.Filtered[Tracer[I], O](base, _right)
//
//    TracingV2(result)
//  }
//
//  def ><[I2, O2](right: TracingV2[I2, O2])(
//      implicit
//      _definedAt: SrcDefinition
//  ): TracingV2[(I, I2), (O, O2)] = {
//
//    val pointwiseResult = Hom.Fn.Pointwise(base, right.base)
//
//    // Pointwise returns Fn[(Var[I], Var[I2]), (O, O2)]
//    // Need to convert to Fn[Var[(I, I2)], (O, O2)]
//    TracingV2(
//      Hom.Fn.Blackbox[Tracer[(I, I2)], (O, O2)](_definedAt) {
//        case tupleVar: Tracer[(I, I2)] =>
//          // Extract I and I2 from tupleVar and apply pointwise
//          val (vi, vi2) = tupleVar.getValue
//          pointwiseResult((Concrete(vi), Concrete(vi2)))
//      }
//    )
//  }
//
//  def -<[O2](right: TracingV2[I, O2])(
//      implicit
//      _definedAt: SrcDefinition
//  ): TracingV2[I, (O, O2)] = {
//
//    val result: Hom.Fn[Tracer[I], (O, O2)] =
//      Hom.Fn.Blackbox[Tracer[I], (O, O2)](_definedAt) { i: Tracer[I] =>
//        val iVal = i.getValue
//        (base(i), right.base(i))
//      }
//
//    TracingV2(result)
//  }
//
//  override protected val unbox: Hom.Fn.K2_[I, O] = primaryForm
//}
//
//object TracingV2 {
//
//  type Traceable
//
//  implicit class TracingFnView[I, O](self: Hom.Fn[Tracer[I], O]) {
//
////    def tracingRun():
//  }
//
//}
