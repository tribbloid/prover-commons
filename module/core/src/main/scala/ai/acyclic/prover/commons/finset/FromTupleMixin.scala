package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.finset.Tuples.{:*, Unit}

trait FromTupleMixin {
  self: Finsets =>

  trait Refine extends Hom.Poly { // poly function

    final val outer = self

    implicit val _0: Unit |- Empty = {
      at[Tuples.Empty] { _ =>
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
}
