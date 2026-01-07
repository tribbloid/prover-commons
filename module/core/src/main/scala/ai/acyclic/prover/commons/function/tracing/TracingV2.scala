package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.function.hom.Hom
import ai.acyclic.prover.commons.multiverse.rewrite.Delegating

case class TracingV2[I, O](
    base: Hom.Fn[Var[I], O]
) extends Delegating[Hom.Fn.K2_[I, O]] {

  @transient lazy val primaryFn: Hom.Fn[I, O] = ???

  override protected val unbox: Hom.Fn.K2_[I, O] = primaryFn
}
