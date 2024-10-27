package ai.acyclic.prover.commons.pending

import ai.acyclic.prover.commons.cap.CanRevokeAll
import ai.acyclic.prover.commons.function.hom.Hom
import ai.acyclic.prover.commons.function.hom.Hom.Circuit
import ai.acyclic.prover.commons.pending.PendingEffect.<<

import scala.language.implicitConversions

private[pending] trait PendingGroup extends CanRevokeAll {

  trait revokeAll_Lvl0 extends Hom.Poly {

    implicit def last[T, C <: PendingEffect]: (T << C) Target T = at[T << C] { v =>
      v.asInstanceOf[T]
    }
  }

  object revokeAll extends revokeAll_Lvl0 {

    implicit def chain[T, R, C <: PendingEffect](
        implicit
        lemma: Circuit[T, R]
    ): (T << C) Target R = at[T << C] { v =>
      lemma(v.asInstanceOf[T])
    }
  }

  case class Annotator[C <: PendingEffect]() {

    object add {

      def apply[V](v: V): V << C = v.asInstanceOf[V << C]
    }

    def <<: : add.type = add

    object revoke {

      def apply[V](v: V << C): V = v.asInstanceOf[V]
    }
  }

  implicit def _annotator[C <: PendingEffect](v: C): Annotator[C] = Annotator[C]()
}
