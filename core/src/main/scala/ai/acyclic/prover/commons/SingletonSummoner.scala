package ai.acyclic.prover.commons

trait SingletonSummoner {

  implicit def summonSingleton[T <: this.type with SingletonSummoner]: T = this.asInstanceOf[T]
}
