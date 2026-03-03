package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.Args

trait DepDomains {

  type In <: Args // Domain, Max

  type Schema = In { type Peer <: In }

  val inputSchema: Schema

  type OutK[T <: In] // Codomain, Min
}
