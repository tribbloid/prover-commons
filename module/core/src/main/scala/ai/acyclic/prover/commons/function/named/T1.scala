package ai.acyclic.prover.commons.function.named

import shapeless.HNil

object T1 extends HigherTier {

  import ai.acyclic.prover.commons.function.PreDef.Named._

  override val lower: T0.type = T0

  // TODO: this will break DefinedAtMixin inference, need some revision
//  implicit def fromScalaFn[I, R](fn: I => R): (I :=> R) = { v =>
//    fn(v.value1)
//  }

  implicit class Fn1Ops[I <: HUB, R](self: Fn[I, R]) {

//    def andThen_direct[R2](g: R => R2): AndThen[R2] = ???
  }

  implicit class Poly1Ops(self: Poly) {

    // TODO: generalize to HigherTier
    object AsShapelessPoly1 extends shapeless.Poly1 {

      implicit def delegate[I, R](
          implicit
          _case: self.Case[I :=> R]
      ): Case.Aux[I, R] = at[I] { ii =>
        _case.argApply(Args(ii :: HNil))
      }
    }
  }
}
