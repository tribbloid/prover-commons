package ai.acyclic.prover.commons.function.bound

trait Domains extends DepDomains {

  type _O
  final type _OK[T] = _O
}
