package ai.acyclic.prover.commons.cap

import ai.acyclic.prover.commons.function.hom.Hom

import scala.language.implicitConversions

private[cap] trait CapabilityGroup extends CanRevokeAll {

  trait WithCap[+C]

  type <>:[+T, +C] <: T & WithCap[C]

  implicit class _ext[T, C](self: T <>: C) {

    def original: T = self
    // TODO: need an API to revoke one specific capability when many are enabled, should be a static function
  }

  trait revokeAll_Imp0 extends Hom.Poly {
    self: Singleton =>

    implicit def last[T, C <: Capability]: (T <>: C) |- T = at[T <>: C].apply { v =>
      v.asInstanceOf[T]
    }
  }

  trait revokeAll extends revokeAll_Imp0 {
    self: Singleton =>

    implicit def chain[T, R, C <: Capability](
        implicit
        lemma: T :=> R
    ): (T <>: C) |- R = at[T <>: C] { v =>
      lemma.asInstanceOf[Hom.Fn.Impl[T, R]].apply(v.asInstanceOf[T]) // fuck scala
    }
  }

  object revokeAll extends revokeAll {}

  case class Annotator[C <: Capability]() {

    object add {

      def apply[V](v: V): V <>: C = v.asInstanceOf[V <>: C]
    }

    // TODO: sometimes left associated function won't work (generic collapse to Nothing), need to file a bug report for it
    def <>: : add.type = add

    object revoke {

      def apply[V](v: V <>: C): V = v.asInstanceOf[V]
    }
  }

  implicit def _annotator[C <: Capability](v: C): Annotator[C] = Annotator[C]()
}
