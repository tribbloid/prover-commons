package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.Thunk
import ai.acyclic.prover.commons.function.FnLike.Derived
import shapeless.HNil

trait HigherTier extends HasMorphism with HasPoly {
//  self: Singleton =>

  import shapeless.::

  val lower: Tier
  type HUB = Any :: lower.HUB

  implicit class FnHigherOps[H <: HUB, +R](self: Function[H, R]) {

    case class curry[HH, HT <: lower.HUB](v: HH)(
        implicit
        ev: (HH :: HT) =:= H
    ) extends lower.DerivedFunction[HT, R](
          { args =>
            val thisArgs = Args[H](ev(args.repr.::(v)))

            self.argsGet(thisArgs)
          }
        )(self)

    case class memoize(args: Args[H])
        extends T0.DerivedFunction[HNil, Thunk[R]](
          { _ =>
            Thunk(() => self.argsGet(args))
          }
        )(self)
  }

  implicit class MorphismHigherOps[H[_] <: HUB, +R[_]](self: Morphism[H, R]) {

    //    case class curry[T, HH[_], HT[_] <: lower.HUB](v: HH[T])(
    //        implicit
    //        ev: (HH :: HT) =:= H[_]
    //    ) extends lower.DerivedMorphic[HT, R](self)(
    //          new Morphic[HT, R] {}
    //        )
    // TODO: don't know how to do it
  }

  implicit class functionIsMorphism[H <: HUB, R](val derivedFrom: Function[H, R])
      extends Morphism[Lambda[t => H], Lambda[t => R]]
      with Derived {

    override def specific[T]: Function[H, R] = derivedFrom
  }

  implicit class morphismIsPoly[
      H[_] <: HUB,
      R[_]
  ](val derivedFrom: Morphism[H, R])
      extends Poly
      with Derived {

    implicit def only[T]: Case[Function[H[T], R[T]]] = forAll[H[T], Any] {
      derivedFrom.specific[T]
    }
  }

  implicit class functionIsPoly[H <: HUB, R](val derivedFrom: Function[H, R]) extends Poly with Derived {

    implicit def only[T]: Case[Function[H, R]] = forAll[H, Any] {
      derivedFrom.specific[T]
    }
  }
}
