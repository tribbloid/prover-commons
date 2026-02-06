package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.finset.Tuples.:*

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
        H_TAIL <: Tuples.Fin,
        TAIL <: Fin,
        HEAD <: VBound
    ](
        implicit
        forTail: H_TAIL |- TAIL
    ): (H_TAIL :* HEAD) |- ><[TAIL, HEAD] = {

      at[H_TAIL :* HEAD] { v =>
        val prev = forTail(v.tail)

        cons(prev, v.head)
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
    *   - (A, B) -> Empty >< A >< B
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
    * similar to [[FromFlatTuple]], but has a fallback case that can convert any type A into `Empty >< A`
    */
  trait FromFlatLowPriority extends Hom.Poly {

    implicit def atomCase[A <: VBound](
        implicit
        refute: shapeless.Refute[shapeless.Generic[A]]
    ): A |- (Empty >< A) = at[A] { a =>
      Empty >< a
    }
  }

  trait FromFlat extends FromFlatTuple with FromFlatLowPriority
  object FromFlat extends FromFlat
}
