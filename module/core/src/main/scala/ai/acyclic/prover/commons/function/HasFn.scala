package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.Same
import ai.acyclic.prover.commons.debug.Debug.CallStackRef

import scala.language.implicitConversions

trait HasFn {

  /**
    * can be anything, but most capable as a [[Args]]
    */
  type IUB

  // TODO: should be protected
  trait FnBase[-I <: IUB] extends FnLike {

    type In >: I <: IUB
    type Out

    /**
      * the only Single Abstract Method interface
      * @param arg
      *   always in Args form
      * @return
      */
    def apply(arg: I): Out
  }

  object FnBase {

    implicit def vanillaToFn[I <: IUB, R](
        fn: I => R
    )(
        implicit
        _definedAt: CallStackRef = CallStackRef.below()
    ): Fn[I, R] = {
      new Fn[I, R] {
        override lazy val definedAt: CallStackRef = _definedAt

        override def apply(arg: I): R = fn(arg)
      }
    }
  }

  type FnCompat[-I <: IUB, +R] = FnBase[I] { type Out <: R }

  trait Fn[I <: IUB, R] extends FnBase[I] { // most specific

    final type In = I
    final type Out = R
  }

  object Fn {

    def apply[I <: IUB, R](
        fn: I => R
    )(
        implicit
        _definedAt: CallStackRef = CallStackRef.below()
    ): Fn[I, R] = {
      FnBase.vanillaToFn(fn)
    }

    case class MadeFrom[I <: IUB, R](raw: FnCompat[I, R])(
        override val references: Seq[FnLike],
        override val name: String
    ) extends Fn[I, R]
        with FnLike.Transparent
        with FnLike.Named {

      def apply(arg: I): R = raw.apply(arg)
    }

    def identity[I <: IUB]: Fn[I, I] = apply(v => v)

    trait Cached[I <: IUB, R] extends Fn[I, R] with FnLike.Transparent1 {

      lazy val sameness: Same.By = Same.ByEquality

      val reference: FnCompat[I, R]

      lazy val correspondence = sameness.Correspondence[I, R]()

      final def apply(key: I): R = {
        correspondence.getOrElseUpdate(key, reference(key))
      }
    }
  }
}
