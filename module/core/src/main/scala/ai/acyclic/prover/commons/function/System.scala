package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.debug.Debug.CallStackRef
import ai.acyclic.prover.commons.function.api.FnLike.Transparent1
import ai.acyclic.prover.commons.function.api.{FnLike, HasMorphism, HasPoly}
import ai.acyclic.prover.commons.same.Same

import scala.language.implicitConversions

object System extends HasMorphism with HasPoly with Serializable {

  type IUB = Any

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
        _lookup: Same.By#Lookup[I, R] = Same.ByEquality.Lookup[I, R]()
    ): FnImpl.Cached[I, R] = {
      new FnImpl.Cached[I, R](self) {

        override lazy val lookup: Same.By#Lookup[I, R] = _lookup
      }
    }
  }

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

  implicit class morphismIsPoly[
      T_/\
  ](val reference: Morphism[T_/\])
      extends Poly
      with Transparent1 {

    import reference._

    implicit def _onlyCase[T <: T_/\]: Case[FnCompat[In[T], Out[T]]] = {
      at[In[T]].apply[Out[T]] { arg =>
        val result: Out[T] = reference.apply[T](arg)
        result
      }
    }
  }

  trait SystemView {

    implicit def asFnSystem(v: this.type): System.type = System
  }

}
