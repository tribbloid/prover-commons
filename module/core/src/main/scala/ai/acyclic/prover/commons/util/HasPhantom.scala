package ai.acyclic.prover.commons.util

trait HasPhantom {

  type Phantom = Phantom.Impl

  object Phantom extends Static.Def {

    /**
      * at the moment, return a null object given a subtype of [[Phantom]]
      *
      * or cause a compilation error if T is not a subtype of [[Phantom]]
      */
    override def get[T <: Impl]: T = null.asInstanceOf[T]
  }
}
