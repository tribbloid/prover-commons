package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.debug.Debug.CallStackRef
import ai.acyclic.prover.commons.same.Same

object PreDef extends FnSystem {

  type IUB = Any

  type :=>[I, R] = Fn[I, R]

  implicit class FnOps[I, R](val self: FnCompat[I, R]) extends Serializable {

    // TODO: enable for all FnLike
    def madeFrom(_name: String)(
        refs: FnLike*
    ): Fn.MadeFrom[I, R] = {

      val result = Fn.MadeFrom[I, R](self)(refs, _name)
      result
    }

    def andThen[R2](g: FnCompat[R, R2])(
        implicit
        _definedAt: CallStackRef = CallStackRef.below(),
        ev: R <:< (R with IUB)
    ): PreDef.Fn[I, R2] = {

      val base: Fn[I, R2] = { ii =>
        val r: R = self.apply(ii)
        val r2: R2 = g.apply(r)
        r2: R2
      }

      base.madeFrom("andThen")(self, g)
    }

    def cachedBy(
        _lookup: Same.By#Lookup[I, R] = Same.ByEquality.Lookup[I, R]()
    ): Fn.Cached[I, R] = {
      new Fn.Cached[I, R](self) {

        override lazy val lookup: Same.By#Lookup[I, R] = _lookup
      }
    }
  }

  type :|~>[-I[_] <: IUB, +R[_]] = MorphismCompat[Any, I, R]
  type :|=>[-I, +R[_]] = DependentCompat[Any, R]

  implicit class MorphismOps[
      T_/\,
      SS <: Morphism[T_/\]
  ](val self: SS) {

    def cachedBy(
        _lookup: Same.By#Lookup[IUB, Any] = Same.ByEquality.Lookup()
    ): Morphism.Cached[T_/\, SS] = {

      type Result = Morphism.Cached[T_/\, SS]

      val result: Result =
        new Morphism.Cached[T_/\, SS](self) {

          override lazy val lookup: Same.By#Lookup[IUB, Any] = _lookup
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
