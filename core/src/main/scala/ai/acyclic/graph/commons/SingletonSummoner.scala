package ai.acyclic.graph.commons

trait SingletonSummoner {

  implicit def summonSingleton[T <: this.type with SingletonSummoner]: T = this.asInstanceOf[T]
}
