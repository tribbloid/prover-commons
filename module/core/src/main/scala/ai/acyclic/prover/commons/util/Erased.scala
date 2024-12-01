package ai.acyclic.prover.commons.util

trait Erased extends AnyRef

object Erased {
  // every type definition in this object reduce to null in runtime
  // TODO: all subtypes should be sealed, otherwise Erased() may fail

  def apply[T <: Erased](): T = null.asInstanceOf[T]
}
