package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.Args

trait DepDomains {
  type In <: Args // Domain, Max

  type OutK[T <: In] // Codomain, Min
}
