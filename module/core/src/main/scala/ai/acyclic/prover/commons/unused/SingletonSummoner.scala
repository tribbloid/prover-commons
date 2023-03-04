package ai.acyclic.prover.commons.unused

trait SingletonSummoner {

  implicit def summonSingleton[T <: this.type with SingletonSummoner]: T = this.asInstanceOf[T]
}
