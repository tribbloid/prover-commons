package ai.acyclic.prover.commons.cap

import ai.acyclic.prover.commons.cap.Pending.<<
import ai.acyclic.prover.commons.function.hom.Hom

private[cap] trait PendingGroup extends CanRevokeAll {

  type Pending

  trait revokeAll_Lvl0 extends Hom.Poly {

    implicit def last[T, C <: Pending]: (T << C) Target T = at[T << C] { v =>
      v.asInstanceOf[T]
    }
  }

  object revokeAll extends revokeAll_Lvl0 {

    implicit def chain[T, R, C <: Pending](
        implicit
        lemma: Compat[T, R]
    ): (T << C) Target R = at[T << C] { v =>
      lemma(v.asInstanceOf[T])
    }
  }
}
