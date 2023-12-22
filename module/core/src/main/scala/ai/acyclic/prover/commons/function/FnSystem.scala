package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.FnLike.Transparent1

abstract class FnSystem extends HasMorphism with HasPoly {

  implicit class morphismIsPoly[
      I[_] <: IUB,
      R[_]
  ](val reference: Morphism[I, R])
      extends Poly
      with Transparent1 {

    implicit def _onlyCase[T]: Case[FnCompat[I[T], R[T]]] = {
      reference.specific[T].enable[BeCase]
    }
  }
}
