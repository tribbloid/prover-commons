package ai.acyclic.prover.commons.function.bound

trait Domains extends DepDomains {

  type Out
  final type OutK[T] = Out

}
