package ai.acyclic.prover.commons

object BoundError {

  trait Law

  class Topology[L <: Law]() {

    final type Law_/\ = L

    implicitly[Law_/\ <:< Law]
  }

  class GraphBuilder[T <: Topology[?]](
      final val tp: T
  ) {

    type Law_/\ = tp.Law_/\

    implicitly[tp.Law_/\ <:< Law]
  }
}
