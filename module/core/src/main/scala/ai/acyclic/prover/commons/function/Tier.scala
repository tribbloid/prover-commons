package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.FnLike.Derived
import ai.acyclic.prover.commons.util.NamedArgs
import shapeless.{HList, SingletonProductArgs}

abstract class Tier {
//  self: Singleton =>
  // CAUTION: DO NOT use Scala 2 tuple form!
  // Scala 3 tuple is equivalent to HList (which itself will be ported to Scala 3 soon)
  // HList also has the extra benefit of being capable of naming its field(s)

  type HUB <: HList

  trait FnDynamics[-I <: HUB, +R] extends SingletonProductArgs {
    self: Fn[I, R] =>

    final def applyProduct(
        args: I
    ): R = {

      self.argsGet(NamedArgs(args))
    }
  }

  trait Fn[-I <: HUB, +R] extends FnLike with FnDynamics[I, R] {
    // always has implicit conversion to a Scala function

    /**
      * the only Single Abstract Method interface
      * @param args
      *   always in Args form
      * @return
      */
    protected def argsApply(args: NamedArgs[I]): R

    def argsGet(args: NamedArgs[I]): R = argsApply(args)
  }

  object Fn {
    // TODO: obviously, function is a morphism
  }

  abstract class DerivedFn[I <: HUB, +R](val impl: Fn[I, R])(
      implicit
      val derivedFrom: FnLike = impl
  ) extends Fn[I, R]
      with Derived {

    final override def argsApply(args: NamedArgs[I]): R = impl.argsGet(args)
  }
}

object Tier {}
