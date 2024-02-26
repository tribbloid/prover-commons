package ai.acyclic.prover.commons.multiverse.rewrite

trait Delegating_Imp0 {
  self: Delegating.type =>

  implicit def unbox1[T]: ConversionPart[Delegating[T], T] =
    (v: Delegating[T]) => Delegating._unbox(v)
}
