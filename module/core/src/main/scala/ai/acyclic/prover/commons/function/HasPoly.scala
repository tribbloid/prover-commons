package ai.acyclic.prover.commons.function

import shapeless.SingletonProductArgs

trait HasPoly extends Tier {

  trait PolyDynamics extends SingletonProductArgs {
    self: Poly =>

    final def applyProduct[H <: HUB, R](args: H)(
        implicit
        _case: Case[Function[H, R]]
    ): R = {

      self.argsApply(Args(args))(_case)
    }
  }

  /**
    * Ad-hoc polymorphic function, the most flexible polymorphism
    *
    * contains several cases, each take a type argument and generate a specific [[Function]]
    *
    * the exact case being selected for function application should be determined in compile-time (by the implicit
    * evidence), doing it in runtime is shunned in type theories (it is fine in set theories tho), but we may still
    * allow it (if not obstructed by type erasure)
    *
    * obviously, both [[Morphism]] and [[Function]] are its trivial examples that only has 1 case
    */
  trait Poly extends PolyLike with PolyDynamics {

    trait BeACase extends FnLike.Cap

    type Case[FF <: Function[_, _]] = FF with FnLike.Can[BeACase]

    def at[FF <: Function[_, _]] = new At[FF]() // same as `at` in Poly1?

    class At[FF <: Function[_, _]] {}

    implicit def forAll[
        I <: HUB,
        O
    ](
        at: At[Function[I, O]]
    ): ForAll[I, O] = new ForAll[I, O]()

    def forAll[I <: HUB, O] = new ForAll[I, O]

    class ForAll[I <: HUB, O] {

      def =>>[OO](fn: Function[I, OO]): Case[Function[I, OO]] = fn.enable[BeACase]

      def apply[OO](fn: Function[I, OO]): Case[Function[I, OO]] = =>>(fn)
    }

    def summon[I <: HUB](
        implicit
        _case: Case[Function[I, _]]
    ): _case.type = _case

    def summonFor[I <: HUB](v: I)(
        implicit
        _case: Case[Function[I, _]]
    ): _case.type = _case

    def argsApply[I <: HUB, R](v: Args[I])(
        implicit
        _case: Case[Function[I, R]]
    ): R = _case.argsGet(v)
  }

}
