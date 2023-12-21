package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.FnLike.Derived
import ai.acyclic.prover.commons.util.NamedArgs
import shapeless.{HList, SingletonProductArgs}

abstract class FnSystem {

  /**
    * can be anything, but most capable as a [[NamedArgs]]
    */
  type Args

}

abstract class Tier extends FnSystem {
//  self: Singleton =>
  // CAUTION: DO NOT use Scala 2 tuple form!
  // Scala 3 tuple is equivalent to HList (which itself will be ported to Scala 3 soon)
  // HList also has the extra benefit of being capable of naming its field(s)

  type HUB <: HList
  override type Args = NamedArgs[HUB]

  trait FnDynamics[-I <: HUB, +R] extends SingletonProductArgs {
    self: FnCompat[I, R] =>

    final def applyProduct(
        args: I
    ): R = {

      self.argsGet(NamedArgs(args))
    }
  }

  trait FnCompat[-I <: HUB, +R] extends FnLike with FnDynamics[I, R] {
    // always has implicit conversion to a Scala function

    type I_/\ >: I
    type R_\/ <: R

    /**
      * the only Single Abstract Method interface
      * @param args
      *   always in Args form
      * @return
      */
    protected def argsApply(args: NamedArgs[I]): R

    def argsGet(args: NamedArgs[I]): R = argsApply(args)
  }

  trait Fn[I <: HUB, R] extends FnCompat[I, R] {

    type I_/\ = I
    type R_\/ = R
  }

  abstract class DerivedFn[I <: HUB, R](val impl: Fn[I, R])(
      implicit
      val derivedFrom: FnLike = impl
  ) extends Fn[I, R]
      with Derived {

    final override def argsApply(args: NamedArgs[I]): R = impl.argsGet(args)
  }
}

object Tier {}
