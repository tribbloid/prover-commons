package ai.acyclic.prover.commons.jit.tracing

trait Inhabited[T] {

  def getExample: T
}

object Inhabited {}
