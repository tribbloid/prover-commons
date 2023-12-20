package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.util.NamedArgs
import shapeless.SingletonProductArgs

import scala.language.implicitConversions

trait HasPoly extends Tier {

  trait PolyDynamics extends SingletonProductArgs {
    self: Poly =>

    final def applyProduct[I <: HUB, R](args: I)(
        implicit
        _case: Case[Fn[I, R]]
    ): R = {

      self.argsApply(NamedArgs(args))(_case)
    }
  }

  /**
    * Ad-hoc polymorphic function, the most flexible polymorphism
    *
    * contains several cases, each take a type argument and generate a specific [[Fn]]
    *
    * the exact case being selected for function application should be determined in compile-time (by the implicit
    * evidence), doing it in runtime is shunned in type theories (it is fine in set theories tho), but we may still
    * allow it (if not obstructed by type erasure)
    *
    * obviously, both [[Morphism]] and [[Fn]] are its trivial examples that only has 1 case
    */
  trait Poly extends PolyLike with PolyDynamics {

    trait BeACase extends FnLike.Cap

    type Case[FF <: Fn[_, _]] = FF with FnLike.Can[BeACase]

    def at[FF <: Fn[_, _]] = new At[FF]() // same as `at` in Poly1?

    class At[FF <: Fn[_, _]] {}

    implicit def forAll[
        I <: HUB,
        R
    ](
        at: At[Fn[I, R]]
    ): ForAll[I, R] = new ForAll[I, R]()

    def forAll[I <: HUB, R] = new ForAll[I, R]

    class ForAll[I <: HUB, R] {

      def =>>[RR](fn: Fn[I, RR]): Case[Fn[I, RR]] = fn.enable[BeACase]

      def apply[RR](fn: Fn[I, RR]): Case[Fn[I, RR]] = =>>(fn)
    }

    def summon[I <: HUB](
        implicit
        _case: Case[Fn[I, _]]
    ): _case.type = _case

    def summonFor[I <: HUB](v: I)(
        implicit
        _case: Case[Fn[I, _]]
    ): _case.type = _case

    def argsApply[I <: HUB, R](v: NamedArgs[I])(
        implicit
        _case: Case[Fn[I, R]]
    ): R = _case.argsGet(v)
  }

}
