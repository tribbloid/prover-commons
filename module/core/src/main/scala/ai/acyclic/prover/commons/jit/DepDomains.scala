package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.Args

trait DepDomains {

  type In <: Args // Domain, Max
  val inputSchema: Args { type Peer <: In }

  type OutK[T <: In] // Codomain, Min
}
