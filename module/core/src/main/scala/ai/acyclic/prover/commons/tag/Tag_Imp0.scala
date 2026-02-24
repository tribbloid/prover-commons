package ai.acyclic.prover.commons.tag

import ai.acyclic.prover.commons.jit.hom.{Const, ConstantFn, Fn, Poly}
import ai.acyclic.prover.commons.jit.eval.Args
import Args.{><:, T0}

private[tag] trait Tag_Imp0 {

  trait revoke_Imp0[CC <: Tag] extends Poly {

    implicit def exactTag[T]: (T <> CC) /=> T = at[T <> CC].apply { v =>
      v.asInstanceOf[T]
    }
  }

  class revoke[CC <: Tag] extends revoke_Imp0[CC] {}

  def revoke[CC <: Tag](
      implicit
      cc: CC = null
  ): revoke[CC] = {
    new revoke[CC]
  }

  object revokeAll extends revoke[Tag] {

//    implicit def subTypeTag[T, CC <: Tag]: (T <> CC) /=> T = at[T <> CC].apply { v =>
//      v.asInstanceOf[T]
//    } // TODO: remove, CC will become Nothing

    implicit def chain[T, R, C <: Tag](
        implicit
        lemma: T |- R
    ): (T <> C) /=> R = at[T <> C] { v =>
      lemma
        .asInstanceOf[Fn.Impl[T ><: T0, R]]
        .apply(Args.><:(Const.Provided(v).asInstanceOf[ConstantFn[T]], Args.eye))
        .asInstanceOf[R] // fuck scala
    }
  }

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
