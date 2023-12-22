package ai.acyclic.prover.commons.function.named

import ai.acyclic.prover.commons.Thunk
import shapeless.HNil

trait HigherTier extends NamedTier {
//  self: Singleton =>

  import shapeless.::

  val lower: NamedTier
  type HUB = Any :: lower.HUB

  implicit class FnHigherOps[I <: HUB, R](self: Fn[I, R]) {

    case class curry[HH, HT <: lower.HUB](v: HH)(
        implicit
        ev: (HH :: HT) =:= I
    ) extends lower.Adjoint.DerivedFn[Args[HT], R](
          { args =>
            val thisArgs = Args[I](ev(args.asHList.::(v)))

            self.argApply(thisArgs)
          }
        )(self)
        with lower.Fn[HT, R]

    case class memoize(args: Args[I])
        extends T0.Adjoint.DerivedFn[Args[HNil], Thunk[R]](
          { _ =>
            Thunk(() => self.argApply(args))
          }
        )(self)
        with T0.Fn[HNil, Thunk[R]]
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
