package ai.acyclic.prover.commons.function.api

import ai.acyclic.prover.commons.debug.Debug.CallStackRef
import ai.acyclic.prover.commons.same.Same

import scala.language.implicitConversions

trait HasFn {

  /**
    * will be Tuple-like in the future
    */
  type IUB

  // TODO: should be protected
  trait Fn[-I <: IUB] extends FnLike {

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

  object Fn {

    protected[function] def apply[I <: IUB, R](
        vanilla: I => R
    )(
        implicit
        definedAt: CallStackRef = definedHere
    ): FnImpl.Defined[I, R] = {

      new FnImpl.Defined[I, R](vanilla, definedAt)
    }
  }

  protected[function] def definedHere: CallStackRef = CallStackRef
    .below(condition = { v =>
      v.isDefinedAtClasses(classOf[HasFn]) || v.isArgDefault
    })
    .below(1)

  implicit def _fromVanilla[I <: IUB, R](
      vanilla: I => R
  ): FnImpl[I, R] = {
    implicit val definedAt: CallStackRef = definedHere
    Fn[I, R](vanilla)
  }

  type FnCompat[-I <: IUB, +R] = Fn[I] { type Out <: R }

  trait FnImpl[I <: IUB, R] extends Fn[I] { // most specific

    final type In = I
    final type Out = R
  }

  object FnImpl {

    class Defined[I <: IUB, R](
        val fn: I => R,
        override val _definedAt: CallStackRef = definedHere
    ) extends FnImpl[I, R] {

      override def apply(arg: I): R = fn(arg)
    }

    case class MadeFrom[I <: IUB, R](raw: FnCompat[I, R])(
        override val references: Seq[FnLike],
        override val name: String
    ) extends FnImpl[I, R]
        with FnLike.Transparent
        with FnLike.Named {

      def apply(arg: I): R = {
        raw.apply(arg)
      }
    }

    def identity[I <: IUB]: FnImpl[I, I] = Fn(v => v)

    class Cached[I <: IUB, R](
        val reference: FnCompat[I, R]
    ) extends FnImpl[I, R]
        with FnLike.Transparent1 {

      lazy val lookup: Same.By#Lookup[I, R] = Same.ByEquality.Lookup[I, R]()

      final def apply(key: I): R = {
        lookup.getOrElseUpdate(key, reference(key))
      }

      final def getExisting(arg: I): Option[R] = {
        lookup
          .get(arg)
      }
    }
  }
}
