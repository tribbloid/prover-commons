package ai.acyclic.prover.commons.function.tracing

trait Inhabited[T] {

  def getExample: T
}

object Inhabited {}
