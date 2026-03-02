package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.Args

trait DepDomains {

  type In <: Args { type Peer <: In } // Domain, Max
  val inputSchema: In

  type OutK[T <: In] // Codomain, Min
}
