package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.FnLike.Derived
import shapeless.{HList, SingletonProductArgs}

abstract class Tier {
//  self: Singleton =>
  // CAUTION: DO NOT use Scala 2 tuple form!
  // Scala 3 tuple is equivalent to HList (which itself will be ported to Scala 3 soon)
  // HList also has the extra benefit of being capable of naming its field(s)

  type HUB <: HList

  trait FnDynamics[-H <: HUB, +R] extends SingletonProductArgs {
    self: Function[H, R] =>

    final def applyProduct(
        args: H
    ): R = {

      self.argsGet(Args(args))
    }
  }

  trait Function[-H <: HUB, +R] extends FnLike with FnDynamics[H, R] {
    // always has implicit conversion to a Scala function

    /**
      * the only Single Abstract Method interface
      * @param args
      *   always in Args form
      * @return
      */
    protected def argsApply(args: Args[H]): R

    def argsGet(args: Args[H]): R = argsApply(args)
  }

  object Function {
    // TODO: obviously, function is a morphism
  }

  abstract class DerivedFunction[H <: HUB, +R](val impl: Function[H, R])(
      implicit
      val derivedFrom: FnLike = impl
  ) extends Function[H, R]
      with Derived {

    final override def argsApply(args: Args[H]): R = impl.argsGet(args)
  }

  type Fn[H <: HUB, +R] = Function[H, R]
}

object Tier {}
