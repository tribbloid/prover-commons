package ai.acyclic.prover.commons.function.bound

import ai.acyclic.prover.commons.util.Phantom

trait PseudoTypeBound extends Phantom with Serializable {

  type Min
  type Max
}
