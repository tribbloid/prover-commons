package ai.acyclic.prover.commons.jit.hom

trait Hom_Imp0 extends HasTypeLambda with Serializable {
  self: Singleton =>

//  implicit def tracerToRepr[I, O](v: TracerCompat[FnCompat[I, O]]): FnRepr[I, O] = FnRepr(v.unbox)
}
