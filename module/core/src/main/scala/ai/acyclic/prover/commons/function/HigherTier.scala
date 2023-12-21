package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.Thunk
import ai.acyclic.prover.commons.function.FnLike.Derived
import ai.acyclic.prover.commons.util.NamedArgs
import shapeless.HNil

trait HigherTier extends HasMorphism with HasPoly {
//  self: Singleton =>

  import shapeless.::

  val lower: Tier
  type HUB = Any :: lower.HUB

  implicit class FnHigherOps[I <: HUB, R](self: FnCompat[I, R]) {

    case class curry[HH, HT <: lower.HUB](v: HH)(
        implicit
        ev: (HH :: HT) =:= I
    ) extends lower.DerivedFn[HT, R](
          { args =>
            val thisArgs = NamedArgs[I](ev(args.asHList.::(v)))

            self.argsGet(thisArgs)
          }
        )(self)

    case class memoize(args: NamedArgs[I])
        extends T0.DerivedFn[HNil, Thunk[R]](
          { _ =>
            Thunk(() => self.argsGet(args))
          }
        )(self)
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

  implicit class fnIsMorphism[I <: HUB, R](val derivedFrom: FnCompat[I, R])
      extends Morphism[Lambda[t => I], Lambda[t => R]]
      with Derived {

    override def specific[T]: FnCompat[I, R] = derivedFrom
  }

  implicit class morphismIsPoly[
      H[_] <: HUB,
      R[_]
  ](val derivedFrom: Morphism[H, R])
      extends Poly
      with Derived {

    implicit def _onlyCase[T]: Case[FnCompat[H[T], R[T]]] = {
      derivedFrom.specific[T].enable[BeACase]
    }
  }

  implicit class functionIsPoly[I <: HUB, R](val derivedFrom: FnCompat[I, R]) extends Poly with Derived {

    implicit def _onlyCase[T]: Case[FnCompat[I, R]] = {
      derivedFrom.specific[T].enable[BeACase]
    }
  }
}
