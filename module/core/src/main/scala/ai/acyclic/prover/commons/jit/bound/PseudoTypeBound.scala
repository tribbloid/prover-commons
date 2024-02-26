package ai.acyclic.prover.commons.jit.bound

import ai.acyclic.prover.commons.util.Phantom

trait PseudoTypeBound extends Phantom with Serializable {

  type Min
  type Max
}
