package ai.acyclic.prover.commons.function

import shapeless.{HList, SingletonProductArgs}
import shapeless.ops.hlist.Tupler
import shapeless.ops.record.Selector
import shapeless.tag.@@

import scala.language.{dynamics, implicitConversions}

abstract class Tier {
  self: Singleton =>
  // CAUTION: DO NOT use Scala 2 tuple form!
  // Scala 3 tuple is equivalent to HList (which itself will be ported to Scala 3 soon)
  // HList also has the extra benefit of being capable of naming its field(s)

  type IUB <: HList

  trait DynamicArgsOps[H <: IUB] extends Dynamic {
    self: Args[H] =>

    def selectDynamic(key: String)(
        implicit
        selector: Selector[H, Symbol @@ key.type]
    ): selector.Out = selector(this.self)
  }

  case class Args[H <: IUB](self: H) extends ArgsLike with DynamicArgsOps[H] {
    // dynamic API, safety delegated to macro

    def asTuple[TT](
        implicit
        tupler: Tupler.Aux[H, TT]
    ): TT = tupler(self)
  }

  object Args {

    implicit def unbox[H <: IUB](v: Args[H]): H = v.self

    implicit def asTuple[H <: IUB, TT](v: Args[H])(
        implicit
        tupler: Tupler.Aux[H, TT]
    ): TT = tupler(v)
  }

  trait DynamicFnOps[I <: IUB, +R] extends SingletonProductArgs {
    self: Function[I, R] =>

    final def applyProduct(
        args: I
    ): R = {

      self.hApply(Args(args))
    }
  }

  trait Function[I <: IUB, +R] extends FnLike with DynamicFnOps[I, R] {
    // always has implicit conversion to a Scala function

    /**
      * the only Single Abstract Method interface
      * @param args
      *   always in Args form
      * @return
      */
    def hApply(args: Args[I]): R
  }

  abstract class Derived[I <: IUB, +R](val from: FnLike)(original: Function[I, R]) extends Function[I, R] with Product {

    final override lazy val productPrefix: String = {
      s"${from}.${this.getClass.getSimpleName}"
    }

    /**
      * the only Single Abstract Method interface
      */
    final override def hApply(args: Args[I]): R = original.hApply(args)
  }

  sealed trait PureTag {
    self: FnLike =>
  }
  trait Pure[T <: IUB, +R] extends Function[T, R] with PureTag {}

  trait Cached[T <: IUB, +R] extends Pure[T, R] {}

  trait Morphism[T] extends shapeless.Poly with FnLike {}
  // shapeless Poly is discontinued in Scala 3, we don't know when it will become available

  trait Transformation[T] extends Morphism[T] {} // as in "natural transformation"

  trait Dependent[T] extends Morphism[T] {}
}

object Tier {}

//object Tier2 extends Tier with Tier.HigherTier {
//
//  override val lower: Tier1.type = Tier1
//}
