package ai.acyclic.prover.commons.cap

import ai.acyclic.prover.commons.function.hom.Hom

private[cap] trait CapabilityBase {

  trait WithCap[+C <: Capability]

//  type <>:[+T, +C <: Capability] = T & WithCap[C]
  // TODO: remove, right associated operator is the source of mandated parentheses

  type <>[+T, +C <: Capability] = T & WithCap[C] // left associated

  trait revoke_Imp0[CC <: Capability] extends Hom.Poly {

    implicit def last[T, C <: CC]: (T <> C) |- T = at[T <> C].apply { v =>
      v.asInstanceOf[T]
    }
  }

  class revoke[CC <: Capability] extends revoke_Imp0[CC] {

    implicit def chain[T, R, C <: CC](
        implicit
        lemma: T :=> R
    ): (T <> C) |- R = at[T <> C] { v =>
      lemma.asInstanceOf[Hom.Fn.Impl[T, R]].apply(v: T) // fuck scala
    }
  }

  def revoke[CC <: Capability](
      implicit
      cc: CC = null
  ): revoke[CC] = {
    new revoke[CC]
  }

  object revokeAll extends revoke[Capability] {}

  implicit class annotator[V](self: V) {

    def add[C <: Capability](
        implicit
        c: C = null
    ): V <> C = self.asInstanceOf[V <> C]

    def <>[C <: Capability](
        implicit
        c: C = null
    ): V <> C = self.asInstanceOf[V <> C]

    def original(
        implicit
        ev: revokeAll.Lemma.At[V]
    ): ev.Out = {

      revokeAll(self)
    }
  }

  def apply[V](v: V) = annotator[V](v)
}
