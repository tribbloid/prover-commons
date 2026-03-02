package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.Args

trait DepDomains {

  type In <: Args // Domain, Max
  val inputSchema: In

  type OutK[T <: inputSchema.Peer] // Codomain, Min
}
