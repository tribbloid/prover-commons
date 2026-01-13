package ai.acyclic.prover.commons.jit.bound

trait Domains extends DepDomains {

  type Out
  final type OutK[T] = Out

}
