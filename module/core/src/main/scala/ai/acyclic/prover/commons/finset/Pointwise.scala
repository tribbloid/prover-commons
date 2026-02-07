package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons.finset.Tuples.:*

trait Pointwise extends Finsets {

  trait Fin
  abstract class Delegate[T <: Tuples.Fin](v: T) extends Fin

  object DUnit extends Delegate(Tuples._0)
  val _Empty = DUnit

  case class DTuple[T <: Tuples.Fin, H <: VBound](v: T :* H) extends Delegate(v)

  // incremental construction of DTuple.
  // it should be implemented such that _Empty >< X >< Y = DTuple(Tuples._0 :* X :* Y)
  type ><[T <: Fin, H] = DTuple[? <: T, ? <: H]
}
