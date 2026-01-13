package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.function.hom.Hom.:=>

case class Id[T]() extends Expr[T :=> T] {
  final type Pending = Any
}
