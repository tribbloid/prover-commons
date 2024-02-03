package ai.acyclic.prover.commons.function.api

import FnLike.Transparent1

trait HasPoly extends HasPolyLike {

  /**
    * Ad-hoc polymorphic function, the most flexible polymorphism
    *
    * contains several cases, each take a type argument and generate a specific [[FnCompat]]
    *
    * the exact case being selected for function application should be determined in compile-time (by the implicit
    * evidence), doing it in runtime is shunned in type theories (it is fine in set theories tho), but we may still
    * allow it (if not obstructed by type erasure)
    *
    * obviously, both [[HasMorphism.Morphism]] and [[FnCompat]] are its trivial examples that only has 1 case
    */
  trait Poly extends PolyLike {
    // TODO: all these cases can only be summoned when Poly is path-dependent, is there an API that works otherwise?
    // TODO: renamed to AdHoc

    def apply[I <: IUB, R](v: I)(
        implicit
        _case: Case[FnCompat[v.type, R]]
    ): R = _case.apply(v)
  }

  implicit class functionIsPoly[I <: IUB, R](val reference: FnCompat[I, R]) extends Poly with Transparent1 {

    implicit def _onlyCase[T]: Case[FnCompat[I, R]] = {
      reference.enable[IsCase]
    }
  }
}
