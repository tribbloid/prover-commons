package ai.acyclic.prover.commons.function.api

import ai.acyclic.prover.commons.cap.Capability.>>

trait HasPolyLike extends HasFn {

  import Explainable._

  trait PolyLike extends Explainable {

    object IsCase extends Explainable.Capability

    type Case[+FF <: Fn[_]] = >>[FF, IsCase.type]

    type At[I <: IUB] = Case[Fn[I]]

    type =>>[I <: IUB, O] = Case[FnImpl[I, O]]

    /**
      * CAUTION: cannot be defined for [[FnCompat]], implicit search will fail
      */

    class CaseBuilder[I <: IUB, F <: Fn[I]] {

      def to[O]: CaseBuilder[I, FnImpl[I, O]] = new CaseBuilder[I, FnImpl[I, O]]
      def =>>[O]: CaseBuilder[I, FnImpl[I, O]] = to[O]

      def defining[FF <: F](fn: FF): Case[FF] = {
        IsCase.>>:(fn)
      }
      def apply[FF <: F](fn: FF): Case[FF] = defining(fn)

      def defining[R](fn: I => R)(
          implicit
          ev: FnImpl[I, R] <:< F
      ): I =>> R = {
        val _fn: FnImpl[I, R] = Fn(fn)
        IsCase.>>:(_fn)
      }
      def apply[R](fn: I => R)(
          implicit
          ev: FnImpl[I, R] <:< F
      ): I =>> R = defining(fn)

      def summon(
          implicit
          _case: Case[F]
      ): _case.type = _case
    }

    // similar to `at` in shapeless Poly1
    def at[I <: IUB] = new CaseBuilder[I, Fn[I]]

    def caseFor[I <: IUB](v: I)(
        implicit
        _case: At[I]
    ): At[I] = _case
  }
}
