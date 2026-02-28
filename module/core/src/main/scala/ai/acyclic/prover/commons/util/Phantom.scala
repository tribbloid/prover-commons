package ai.acyclic.prover.commons.util

abstract class Phantom(val v: Unit = ()) extends AnyVal

object Phantom extends Static.Group {

  type Impl = Phantom

  /**
    * at the moment, return a null object given a subtype of [[Phantom]]
    *
    * or cause a compilation error if T is not a subtype of [[Phantom]]
    */
  override def get[T <: Impl]: T = ().asInstanceOf[T]
}
