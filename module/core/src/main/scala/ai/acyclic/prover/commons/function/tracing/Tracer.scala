package ai.acyclic.prover.commons.function.tracing

trait Tracer[-I, +O] {}

object Tracer {

  type Endo[T] = Tracer[T, T]

  type Gen[+T] = Tracer[Any, T]

  {
    implicitly[Gen[Int] <:< Endo[Int]]
  }
}
