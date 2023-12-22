package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.FnLike.Transparent1

trait HasFn {

  /**
    * can be anything, but most capable as a [[Args]]
    */
  type IUB

  trait FnCompat[-I <: IUB, +R] extends FnLike {

    type I_/\ >: I
    type O_\/ <: R

    /**
      * the only Single Abstract Method interface
      * @param args
      *   always in Args form
      * @return
      */
    protected def doArgApply(args: I): R

    def argApply(args: I): R = doArgApply(args)
  }

  trait Fn[I <: IUB, R] extends FnCompat[I, R] { // most specific

    type I_/\ = I
    type O_\/ = R
  }

  abstract class DerivedFn[I <: IUB, R](val impl: Fn[I, R])(
      implicit
      val reference: FnLike = impl
  ) extends Fn[I, R]
      with Transparent1 {

    final override def doArgApply(args: I): R = impl.argApply(args)
  }

//  type Fn[I <: IUB, R] <: FnBase[I, R]

}
