package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.util.Erased

trait PseudoTypeBound extends Erased {

  type Min
  type Max
}
