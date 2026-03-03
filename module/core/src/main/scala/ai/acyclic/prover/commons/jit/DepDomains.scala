package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.Args

trait DepDomains {

  type In <: Args // Domain, Max

  type _In = In { type Peer <: In }

//  type Schema = In { type Peer <: In }

  val inputSchema: _In

  type OutK[T <: _In] // Codomain, Min
}
