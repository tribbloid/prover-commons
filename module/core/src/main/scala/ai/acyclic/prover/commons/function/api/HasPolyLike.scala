package ai.acyclic.prover.commons.function.api

import ai.acyclic.prover.commons.cap.Capability.<>

trait HasPolyLike extends HasFn {

  import Explainable._

  trait FnBuilder[I <: IUB, O] {

    type =>>[i <: IUB, o] <: FnImpl[i, o] {
//      type In = i
//      type Out = o
    }

    def mixin[i <: IUB, o](v: FnImpl[i, o]): =>>[i, o]

    type Copy[i <: IUB, o] <: FnBuilder[i, o]
    protected def copy[i <: IUB, o]: Copy[i, o]

    def at[i <: IUB]: Copy[i, O] = copy

    def to[o]: Copy[I, o] = copy
    final def =>>[o]: Copy[I, o] = to

    def defining[R <: O](fn: I => R): I =>> R = {
      val _fn: FnImpl[I, R] = Fn(fn)
      mixin(_fn)
    }
    final def apply[R <: O](fn: I => R): I =>> R = defining(fn)
  }

  trait PolyLike extends Explainable {

    object IsCase extends Explainable.Capability

    type Case[+FF <: Fn[_]] = <>[FF, IsCase.type]

    type At[I <: IUB] = Case[Fn[I]]
    type Compat[I <: IUB, O] = Case[FnCompat[I, O]]
    type =>>[I <: IUB, O] = Case[FnImpl[I, O]]

    class CaseBuilder[I <: IUB, O] extends FnBuilder[I, O] {

      type Copy[i <: IUB, o] = CaseBuilder[i, o]
      override protected def copy[i <: IUB, o]: Copy[i, o] = {
        this.asInstanceOf[CaseBuilder[i, o]]
      }

      override def mixin[i <: IUB, o](v: FnImpl[i, o]): i =>> o = {

        v <>: IsCase
      }

      type =>>[i <: IUB, o] = PolyLike.this.=>>[i, o]

      def summon(
          implicit
          _case: Compat[I, O]
      ): _case.type = _case
    }

    // similar to `at` in shapeless Poly1
    def at[I <: IUB] = new CaseBuilder[I, Any]
  }
}
