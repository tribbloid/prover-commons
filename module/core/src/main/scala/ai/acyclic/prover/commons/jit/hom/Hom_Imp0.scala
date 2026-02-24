package ai.acyclic.prover.commons.jit.hom

import ai.acyclic.prover.commons.jit.Hom

trait Hom_Imp0 extends HasTypeLambda with Serializable {
  self: Hom.type =>

//  implicit def tracerToRepr[I, O](v: TracerCompat[FnCompat[I, O]]): FnRepr[I, O] = FnRepr(v.unbox)
}
