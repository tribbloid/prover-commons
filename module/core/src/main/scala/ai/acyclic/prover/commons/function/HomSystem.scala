package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.collection.CacheView
import ai.acyclic.prover.commons.debug.Debug.CallStackRef
import ai.acyclic.prover.commons.function.api.FnLike
import ai.acyclic.prover.commons.same.Same

import scala.language.implicitConversions

object HomSystem extends HomSystem_Imp0 {

  implicit class FnOps[I, R](val self: FnCompat[I, R]) extends Serializable {

    // TODO: enable for all FnLike
    def madeFrom(_name: String)(
        refs: FnLike*
    ): FnImpl.MadeFrom[I, R] = {

      val result = FnImpl.MadeFrom[I, R](self)(refs, _name)
      result
    }

    def andThen[R2](g: Fn[R] { type Out = R2 })(
        implicit
        _definedAt: CallStackRef = CallStackRef.below(),
        ev: R <:< (R with IUB)
    ): FnImpl[I, R2] = {

      val base: FnImpl[I, R2] = Impl { ii: I =>
        val r: R = self.apply(ii)
        val r2: R2 = g.apply(r)
        r2
      }

      base.madeFrom("andThen")(self, g)
    }

    def cachedBy(
        cache: CacheView[I, R] = Same.ByEquality.Lookup[I, R]()
    ): FnImpl.Cached[I, R] = {
      new FnImpl.Cached[I, R](self) {

        override lazy val underlyingCache: CacheView[I, R] = cache
      }
    }
  }

  implicit class MonoOps[
      T_/\,
      SS <: Mono[T_/\]
  ](val self: SS) {

    def cachedBy(
        _lookup: CacheView[IUB, Any] = Same.ByEquality.Lookup()
    ): Mono.Cached[T_/\, SS] = {

      type Result = Mono.Cached[T_/\, SS]

      val result: Result =
        new Mono.Cached[T_/\, SS](self) {

          override lazy val lookup: CacheView[IUB, Any] = _lookup
        }
      result

    }
  }

  implicit class PolyOps[P <: Poly](self: P) {

    def apply[I, R](arg: I)(
        implicit
        _case: self.Case[FnCompat[I, R]]
    ): R = self.apply(arg)(_case)

    //    def cached: Same.ByEquality.CachedPoly[P] = Same.ByEquality.CachedPoly(self)
  }

  trait SystemView {

    implicit def asFnSystem(v: this.type): HomSystem.type = HomSystem
  }

}
