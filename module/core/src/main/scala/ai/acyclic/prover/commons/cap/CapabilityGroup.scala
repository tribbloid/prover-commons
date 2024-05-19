package ai.acyclic.prover.commons.cap

import ai.acyclic.prover.commons.cap.Capability.<>
import ai.acyclic.prover.commons.function.Hom

trait CapabilityGroup extends CanRevokeAll {

  type Capability

  trait revokeAll_Imp0 extends Hom.Poly {

    implicit def last[T, C <: Capability]: (T <> C) =>> T = at[T <> C] { v =>
      v.asInstanceOf[T]
    }
  }

  trait revokeAll extends revokeAll_Imp0 {

    implicit def chain[T, R, C <: Capability](
        implicit
        lemma: Compat[T, R]
    ): (T <> C) =>> R = at[T <> C] { v =>
      lemma(v.asInstanceOf[T])
    }

  }

  object revokeAll extends revokeAll
}
