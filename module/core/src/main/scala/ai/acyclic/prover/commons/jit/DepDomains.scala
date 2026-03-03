package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.{ArgSchema, Args}

trait DepDomains {

  type In <: Args // Domain, Max

//  type Schema = In { type Peer <: In }

  val inputSchema: ArgSchema[In]

  type OutK[T <: In] // Codomain, Min
}
