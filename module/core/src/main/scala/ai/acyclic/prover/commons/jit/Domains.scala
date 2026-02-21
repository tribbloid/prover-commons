package ai.acyclic.prover.commons.jit

trait Domains extends DepDomains {

  type Out
  final type OutK[T] = Out

}
