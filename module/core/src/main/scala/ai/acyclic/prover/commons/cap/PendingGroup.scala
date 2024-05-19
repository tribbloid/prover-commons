package ai.acyclic.prover.commons.cap

import ai.acyclic.prover.commons.cap.Pending.<<
import ai.acyclic.prover.commons.function.Hom

trait PendingGroup extends CanRevokeAll {

  type Pending

  trait revokeAll_Lvl0 extends Hom.Poly {

    implicit def last[T, C <: Pending]: (T << C) =>> T = at[T << C] { v =>
      v.asInstanceOf[T]
    }
  }

  object revokeAll extends revokeAll_Lvl0 {

    implicit def chain[T, R, C <: Pending](
        implicit
        lemma: Compat[T, R]
    ): (T << C) =>> R = at[T << C] { v =>
      lemma(v.asInstanceOf[T])
    }
  }
}
