package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.FnLike.Transparent1

trait HasPolyLike extends HasFn {

  trait PolyLike extends FnLike {

    trait IsCase extends FnLike.Cap
    // TODO: should be a type

    type Case[+FF <: FnBase[_]] = FF with FnLike.Can[IsCase]

    type =>>[I <: IUB, O] = Case[Fn[I, O]]
    type CaseFrom[I <: IUB] = Case[FnBase[I]]

    class CaseBuilder[I <: IUB, F <: FnBase[I]] {

      def to[O]: CaseBuilder[I, FnCompat[I, O]] = new CaseBuilder[I, FnCompat[I, O]]
      def =>>[O]: CaseBuilder[I, FnCompat[I, O]] = to[O]

      def defining[FF <: F](fn: FF): Case[FF] = fn.enable[IsCase]
      def apply[FF <: F](fn: FF): Case[FF] = defining(fn)

      def defining[R](fn: I => R)(
          implicit
          ev: Fn[I, R] <:< F
      ): Case[Fn[I, R]] = Fn(fn).enable[IsCase]
      def apply[R](fn: I => R)(
          implicit
          ev: Fn[I, R] <:< F
      ): Case[Fn[I, R]] = defining(fn)

      def summon(
          implicit
          _case: Case[F]
      ): _case.type = _case
    }

    // similar to `at` in shapeless Poly1
    def at[I <: IUB] = new CaseBuilder[I, FnBase[I]]

    def getCaseFor[I <: IUB](v: I)(
        implicit
        _case: Case[FnBase[I]]
    ): _case.type = _case
  }
}

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
