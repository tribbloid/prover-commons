package ai.acyclic.prover.commons.util

trait Phantom extends AnyRef

object Phantom {
  // phantom type, never instantiate in runtime
  // every type definition in this object reduce to null in runtime
  // TODO: all subtypes should be sealed, otherwise Erased() may fail

  def apply[T <: Phantom](): T = null.asInstanceOf[T]
}
