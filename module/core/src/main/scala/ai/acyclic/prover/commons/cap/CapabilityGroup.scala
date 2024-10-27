package ai.acyclic.prover.commons.cap

import ai.acyclic.prover.commons.function.hom.Hom

import scala.language.implicitConversions

private[cap] trait CapabilityGroup extends CanRevokeAll {

  trait _Can[+C]

  type <>[+T, +C] <: T with _Can[C]

  trait revokeAll_Imp0 extends Hom.Poly {

    implicit def last[T, C <: Capability]: (T <> C) Target T = at[T <> C].apply { v =>
      v.asInstanceOf[T]
    }
  }

  trait revokeAll extends revokeAll_Imp0 {

    implicit def chain[T, R, C <: Capability](
        implicit
        lemma: Compat[T, R]
    ): (T <> C) Target R = at[T <> C] { v =>
      lemma(v.asInstanceOf[T])
    }

  }

  object revokeAll extends revokeAll

  case class Annotator[C <: Capability]() {

    object add {

      def apply[V](v: V): V <> C = v.asInstanceOf[V <> C]
    }

    // TODO: sometimes left associated function won't work (generic collapse to Nothing), need to file a bug report for it
    def <>: : add.type = add

    object revoke {

      def apply[V](v: V <> C): V = v.asInstanceOf[V]
    }
  }

  implicit def _annotator[C <: Capability](v: C): Annotator[C] = Annotator[C]()
}
