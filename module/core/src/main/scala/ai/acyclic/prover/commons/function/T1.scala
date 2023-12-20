package ai.acyclic.prover.commons.function

object T1 extends HigherTier {

  import Symbolic._

  override val lower: T0.type = T0

  implicit class Fn1Ops[H <: HUB, R](self: Function[H, R]) {

    case class AndThen[R2](g: R :=> R2)
        extends DerivedFunction[H, R2](
          { args =>
            val r = self.argsGet(args)
            val r2 = g.apply(r)
            r2
          }
        )(self)

    def andThen[R2](g: R :=> R2): AndThen[R2] = AndThen(g)

    def andThen2[R2](g: R => R2): AndThen[R2] = ???
  }

  implicit class Poly1Ops(self: Poly) {

//    object AsShapelessPoly1 extends shapeless.Poly1 {
//
//      implicit def delegate[I, R](
//          implicit
//          _case: self.Case[I :=> R]
//      ): Case.Aux[I, R] = at[I] { ii =>
//        _case.repr.argsGet(Args(ii :: HNil))
//      }
//    }
  }
}
