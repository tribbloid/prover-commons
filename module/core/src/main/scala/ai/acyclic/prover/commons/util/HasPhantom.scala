package ai.acyclic.prover.commons.util

trait HasPhantom {

  type Phantom = Phantom.Impl

  object Phantom extends Static.Def {

    override def get[T <: Impl]: T = null.asInstanceOf[T]

    def apply[T <: Phantom](): T = get[T]
  }
}
