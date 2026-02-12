package ai.acyclic.prover.commons.jit.bound

trait DepDomains {

  type In // Domain, Max
  type OutK[T <: In] // Codomain, Min
}
