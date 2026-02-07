package ai.acyclic.prover.commons.multiverse.rewrite

/**
  * Mimic the behaviour of delegate feature in Kotlin.
  *
  * allowing methods of [[unbox]] to be called on this object directly
  *
  * if [[unbox]] is also a Delegating, then methods of its [[unbox]] can be called on this object recursively
  *
  * @tparam T
  *   type of the object which this object can delegate to
  */
trait Delegating[+T] {
  // TODO: can be merged into "multiverse.CanNormalise"

  protected val unbox: T
}

object Delegating extends Delegating_Imp0 with HasCoercion {

  def _unbox[T](v: Delegating[T]): T = v.unbox
}
