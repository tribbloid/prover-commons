package ai.acyclic.prover.commons.function.named

import ai.acyclic.prover.commons.Thunk
import shapeless.HNil

trait HigherTier extends NamedTier {
//  self: Singleton =>

  import shapeless.::

  val lower: NamedTier
  type HUB = Any :: lower.HUB

  implicit class FnHigherOps[I <: HUB, R](self: Fn[I, R]) {

    def curry[HH, HT <: lower.HUB](v: HH)(
        implicit
        ev: (HH :: HT) =:= I
    ) = lower.Fn {

      val src = self.self
      case object curry
          extends lower.Adjoint.DerivedFn[Args[HT], R](
            { args =>
              val thisArgs = Args[I](ev(args.asHList.::(v)))

              src(thisArgs)
            }
          )(src)

      curry
    }

    def memoize(args: Args[I]) = T0.Fn {

      val src = self.self

      case object memoize
          extends T0.Adjoint.DerivedFn[Args[HNil], Thunk[R]](
            { _ =>
              Thunk(() => src.apply(args))
            }
          )(src)

      memoize
    }

  }

  implicit class MorphismHigherOps[H[_] <: HUB, R[_]](self: Morphism[H, R]) {

    //    case class curry[T, HH[_], HT[_] <: lower.HUB](v: HH[T])(
    //        implicit
    //        ev: (HH :: HT) =:= H[_]
    //    ) extends lower.DerivedMorphic[HT, R](self)(
    //          new Morphic[HT, R] {}
    //        )
    // TODO: don't know how to do it
  }

}
