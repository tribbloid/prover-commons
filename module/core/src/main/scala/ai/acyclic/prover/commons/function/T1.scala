package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.util.NamedArgs
import shapeless.HNil


object T1 extends HigherTier {

  import Symbolic._

  override val lower: T0.type = T0

  // TODO: this will break DefinedAtMixin inference, need some revision
//  implicit def fromScalaFn[I, R](fn: I => R): (I :=> R) = { v =>
//    fn(v.value1)
//  }

  implicit class Fn1Ops[I <: HUB, R](self: FnCompat[I, R]) {

    case class AndThen[R2](g: R :=> R2)
        extends DerivedFn[I, R2](
          { args =>
            val r = self.argsGet(args)
            val r2 = g.apply(r)
            r2
          }
        )(self)

    def andThen[R2](g: R :=> R2): AndThen[R2] = AndThen(g)

    def andThen_direct[R2](g: R => R2): AndThen[R2] = ???
  }

  implicit class Poly1Ops(self: Poly) {

    object AsShapelessPoly1 extends shapeless.Poly1 {

      implicit def delegate[I, R](
          implicit
          _case: self.Case[I :=> R]
      ): Case.Aux[I, R] = at[I] { ii =>
        _case.argsGet(NamedArgs(ii :: HNil))
      }
    }
  }
}
