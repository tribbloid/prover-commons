package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.{Args, Conformal}

trait DepDomains {

  type In <: Args { type Peer = In } // Domain, Max

  type OutK[T <: In] // Codomain, Min
}
