package ai.acyclic.prover.commons.tag

import ai.acyclic.prover.commons.function.hom.Hom

private[tag] trait Tag_Imp0 {

  trait revoke_Imp0[CC <: Tag] extends Hom.Poly {

    implicit def last[T, C <: CC]: (T <> C) |- T = at[T <> C].apply { v =>
      v.asInstanceOf[T]
    }
  }

  class revoke[CC <: Tag] extends revoke_Imp0[CC] {

    implicit def chain[T, R, C <: CC](
        implicit
        lemma: T :=> R
    ): (T <> C) |- R = at[T <> C] { v =>
      lemma.asInstanceOf[Hom.Fn.Impl[T, R]].apply(v: T) // fuck scala
    }
  }

  def revoke[CC <: Tag](
      implicit
      cc: CC = null
  ): revoke[CC] = {
    new revoke[CC]
  }

  object revokeAll extends revoke[Tag] {}

  implicit class annotator[V](self: V) {

    def add[C <: Tag](
        implicit
        c: C = null
    ): V <> C = self.asInstanceOf[V <> C]

    def <>[C <: Tag](
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
