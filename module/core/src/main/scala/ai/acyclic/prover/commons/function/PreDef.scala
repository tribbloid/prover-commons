package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.Same
import ai.acyclic.prover.commons.debug.Debug.CallStackRef

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

    type Cached = Same.ByEquality.CachedFn[I, R]

    def cached: Cached = Same.ByEquality.CachedFn(self)
  }

  type :|~>[-I[_] <: IUB, +R[_]] = Morphism[I, R]
  type :|=>[-I, +R[_]] = Dependent[I, R]

  implicit class MorphismOps[-I[_] <: IUB, +R[_]](self: Morphism[I, R]) {

    def apply[T](args: I[T]): R[T] = self.apply(args)
  }

  implicit class PolyOps(self: Poly) {

    def apply[I, R](arg: I)(
        implicit
        _case: self.Case[FnCompat[I, R]]
    ): R = self.apply(arg)(_case)
  }

}
