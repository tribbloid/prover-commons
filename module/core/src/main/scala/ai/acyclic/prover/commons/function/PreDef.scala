package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.named.{T0, T1}
import shapeless.{::, HNil}

object PreDef {

  object Adjoint extends FnSystem {
    type IUB = Any
  }

  trait Fn[I, R] extends Adjoint.Fn[I, R] {

    def apply(args: I): R = argApply(args)
  }
  type :=>[I, R] = Fn[I, R]

  implicit class _AndThenOps[I, R](self: Adjoint.FnCompat[I, R]) {

    case class andThen[R2](g: Adjoint.FnCompat[R, R2])
        extends Adjoint.DerivedFn[I, R2](
          { ii =>
            val r = self.argApply(ii)
            val r2 = g.argApply(r)
            r2
          }
        )(self)
        with Fn[I, R2]
  }

  trait Morphism[-I[_], +R[_]] extends Adjoint.Morphism[I, R] {

    def apply[T](args: I[T]): R[T] = argApply(args)
  }
  type :|~>[-I[_], +R[_]] = Morphism[I, R]

  type Dependent[-I, +R[_]] = Morphism[Lambda[t => I], R]
  type :|=>[-I, +R[_]] = Dependent[I, R]

  trait Poly extends Adjoint.Poly {

    def apply[I, R](arg: I)(
        implicit
        _case: Case[Adjoint.FnCompat[I, R]]
    ): R = argApply(arg)(_case)
  }

  trait Named {

    type :=[R] = T0.Fn[HNil, R]
    type Fn0[R] = :=[R]

    type :=>[I, R] = T1.Fn[I :: HNil, R]
    type Fn1[I, R] = :=>[I, R]

    type :|=>[-I, +R[_]] = T1.Dependent[I :: HNil, R]

    type :|~>[-I[_], +R[_]] = T1.Morphism[Lambda[t => I[t] :: HNil], R]

    type Poly1 = T1.Poly
  }

  object Named extends Named {}
}
