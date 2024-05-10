package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.collection.CacheView
import ai.acyclic.prover.commons.same.Same

import scala.language.implicitConversions

object HomSystem extends HomBase {

  override type Arg1[T] = T
  override type _Unit = Unit

  override def arg1[T](v: T): T = v
  override val _unit: Unit = ()

  implicit class FnOps[I, R](val self: FnCompat[I, R]) extends Serializable {

    def cachedBy(
        cache: CacheView[I, R] = Same.ByEquality.Lookup[I, R]()
    ): Fn.Cached[I, R] = {
      new Fn.Cached[I, R](self) {

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

}
