package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.finset.Tuples.><:
import shapeless.::

trait FromTupleMixin {
  self: Finsets =>

  trait Refine extends Hom.Poly { // poly function

    final val outer = self

    implicit val _0: shapeless.HNil |- Empty = {
      at[shapeless.HNil] { _ =>
        Empty
      }
    }

    implicit def _inductive[
        HEAD <: VBound,
        H_TAIL <: Tuples.Fin,
        TAIL <: Fin
    ](
        implicit
        forTail: H_TAIL |- TAIL
    ): (HEAD :: H_TAIL) |- (HEAD ><: TAIL) = {

      at[HEAD :: H_TAIL] { v =>
        val prev = forTail(v.tail)

        cons(v.head, prev)
      }
    }
  }

  trait FromTuple[B <: VBound] extends Refine {}
  object FromTuple extends FromTuple[VBound] {}

  object FromTuple_Singleton extends FromTuple[VBound & Singleton] {}

  /**
    * A polymorphic function that takes a Scala tuple or Unit and convert to a [[Fin]]
    *
    * e.g.
    *   - (A, B) -> A ><: B ><: Empty
    *   - Unit -> Empty
    */
  trait FromFlatTuple extends Hom.Poly {

    implicit def unitCase: Unit |- Tuples._0 = at[Unit] { _ =>
      Tuples._0
    }

    implicit def tupleCase[
        P,
        L <: shapeless.HList,
        Out <: Fin
    ](
        implicit
        gen: shapeless.Generic.Aux[P, L],
        fromTuple: FromTuple.Impl[L, Out]
    ): P |- Out = at[P] { v =>
      val l = gen.to(v)
      fromTuple(l)
    }
  }
  object FromFlatTuple extends FromFlatTuple

  /**
    * similar to [[FromFlatTuple]], but has a fallback case that can convert any type A into `A ><: Empty`
    */
  trait FromFlatLowPriority extends Hom.Poly {

    implicit def atomCase[A <: VBound](
        implicit
        refute: shapeless.Refute[shapeless.Generic[A]]
    ): A |- (A ><: Empty) = at[A] { a =>
      a ><: Empty
    }
  }

  trait FromFlat extends FromFlatTuple with FromFlatLowPriority
  object FromFlat extends FromFlat
}
