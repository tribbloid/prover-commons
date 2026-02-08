package ai.acyclic.prover.commons.jit.bound

import ai.acyclic.prover.commons.util.Phantom

trait PseudoTypeBound extends Phantom with Serializable { // TODO: merge into TypeBound

  type Min
  type Max
}
