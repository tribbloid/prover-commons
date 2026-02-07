package ai.acyclic.prover.commons.finset

trait Pointwise extends Finsets {

  trait Fin
  abstract class Delegate[T <: Tuples.Fin](v: T) extends Fin

  object DUnit extends Delegate(Tuples._0)
  val _Empty = DUnit

  case class DTuple[T <: Tuples.Fin, H <: VBound](v: Tuples.><[T, H]) extends Delegate(v)
  type ><[T <: Fin, H] = DTuple[? <: T, ? <: H]
}
