package ai.acyclic.prover.commons.function.tracing

trait Tracer[+O] {}

object Tracer {

  type Endo[T] = Tracer[T]

}
