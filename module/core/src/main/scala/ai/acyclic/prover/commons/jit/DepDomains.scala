package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.Args

trait DepDomains {

  type In <: Args // Domain, Max

  val noInput: In

  type OutK[T <: In] // Codomain, Min
}
