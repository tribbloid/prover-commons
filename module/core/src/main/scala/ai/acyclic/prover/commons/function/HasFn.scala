package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.FnLike.Transparent1

import scala.language.implicitConversions

trait HasFn {

  /**
    * can be anything, but most capable as a [[Args]]
    */
  type IUB

  trait FnBase[-I <: IUB] extends FnLike {

    type In >: I <: IUB
    type Out

    /**
      * the only Single Abstract Method interface
      * @param arg
      *   always in Args form
      * @return
      */
    protected def _argApplySAM(arg: I): Out

    def apply(arg: I): Out = _argApplySAM(arg)
  }

  type FnCompat[-I <: IUB, +R] = FnBase[I] { type Out <: R }

  trait Fn[I <: IUB, R] extends FnBase[I] { // most specific

    final type In = I
    final type Out = R
  }

  implicit def vanillaToFn[I <: IUB, R](
      fn: I => R
  ): Fn[I, R] = {
    new Fn[I, R] {
      override protected def _argApplySAM(arg: I): R = fn(arg)
    }
  }

  abstract class DerivedFn[I <: IUB, R](val impl: Fn[I, R])(
      implicit
      val reference: FnLike = impl
  ) extends Fn[I, R]
      with Transparent1 {

    final override def _argApplySAM(arg: I): R = impl.apply(arg)
  }

//  type Fn[I <: IUB, R] <: FnBase[I, R]

}
