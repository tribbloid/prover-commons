package ai.acyclic.prover.commons.cap

import ai.acyclic.prover.commons.cap.Capability.>>
import ai.acyclic.prover.commons.function.Hom

trait CapabilityGroup extends CanRevokeAll {

  type Capability

  trait revokeAll_Lvl0 extends Hom.Poly {

    implicit def last[T, C <: Capability]: (T >> C) =>> T = at[T >> C] { v =>
      v.asInstanceOf[T]
    }
  }

  object revokeAll extends revokeAll_Lvl0 {

    implicit def chain[T, C <: Capability](
        implicit
        lemma: At[T]
    ): (T >> C) =>> lemma.Out = at[T >> C] { v =>
      lemma(v.asInstanceOf[T])
    }
  }
}
