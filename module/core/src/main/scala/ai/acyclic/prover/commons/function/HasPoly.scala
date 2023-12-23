package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.FnLike.Transparent1

trait HasPoly extends HasFn {

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

    trait BeCase extends FnLike.Cap

    type Case[+FF <: FnBase[_]] = FF with FnLike.Can[BeCase]

    type =>>[I <: IUB, O] = Case[Fn[I, O]]
    type CaseFrom[I <: IUB] = Case[FnBase[I]]

    class CaseBuilder[F <: FnBase[_]] {

      def apply[FF <: F](fn: FF): Case[FF] = fn.enable[BeCase]

      def =>>[FF <: F](fn: FF): Case[FF] = fn.enable[BeCase]

      def summon(
          implicit
          _case: Case[F]
      ): _case.type = _case
    }

    def forCase[F <: FnBase[_]] = new CaseBuilder[F]()

    // similar to `at` in shapeless Poly1
    def at[I <: IUB]: CaseBuilder[FnBase[I]] = forCase[FnBase[I]]

    class BackupCaseBuilder[I <: IUB, F <: FnBase[I]] {

      def =>>[R](fn: I => R)(
          implicit
          ev: Fn[I, R] <:< F
      ): Case[Fn[I, R]] = FnBase.vanillaToFn(fn).enable[BeCase]

    }

    // TODO: this is only for backward compatibility, remove it
    def forAll[I <: IUB] = new BackupCaseBuilder[I, FnBase[I]]

    def under[I <: IUB]: CaseBuilder[FnBase[I]] = forCase[FnBase[I]]

    def summonFor[I <: IUB](v: I)(
        implicit
        _case: Case[FnBase[I]]
    ): _case.type = _case

    def apply[I <: IUB, R](v: I)(
        implicit
        _case: Case[FnCompat[I, R]]
    ): R = _case.apply(v)
  }

  implicit class functionIsPoly[I <: IUB, R](val reference: FnCompat[I, R]) extends Poly with Transparent1 {

    implicit def _onlyCase[T]: Case[FnCompat[I, R]] = {
      reference.enable[BeCase]
    }
  }

}
