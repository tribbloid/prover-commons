package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.cap.Capability
import ai.acyclic.prover.commons.cap.Capability.<>

import scala.language.implicitConversions

object Magnet {

  // this is cap-based, has Option overhead, should be replaced with `XX | Null` in the future
  type OptionMagnet[+T] = Option[T] <> OptionMagnetCap.type

  case object OptionMagnetCap extends Capability {

    implicit def enable[T](v: Option[T]): OptionMagnet[T] = {

      v <>: this
    }

    implicit def box[T](v: T): OptionMagnet[T] = {

      Option(v) <>: this
    }

//    implicit class Unbox[T](self: OptionMagnet[T]) {
//
//      def unbox: Option[T] = self
//    }

    // TODO: this doesn't work, Null is excluded from conversion
//    implicit def none(v: Null): None.type <> OptionMagnetCap.type = {
//
//      None <>: this
//    }
  }

  // ---

  type PreferRightMagnet[+L, +R] = Either[L, R] <> PreferRightCap.type

  trait PreferRightCap_Imp0 extends Capability {

    implicit def boxLeft[L](v: L): Left[L, Nothing] <> PreferRightCap.type = {

      val left: Left[L, Nothing] = Left[L, Nothing](v)

      val result = left <>: PreferRightCap
      result
    }
  }

  case object PreferRightCap extends PreferRightCap_Imp0 {

    implicit def boxRight[R](v: R): Right[Nothing, R] <> this.type = {

      Right(v) <>: PreferRightCap
    }

    implicit def enable[L, R](v: Either[L, R]): PreferRightMagnet[L, R] = {

      v <>: this
    }

//    implicit class Unbox[L, R](self: PreferRightMagnet[L, R]) {
//
//      def unbox: Either[L, R] = self
//    }
  }

  // `=>` is not sealed, no need to use capability tagging
  trait ComputeMagnet[+R] extends (() => R) {

    final def get: R = apply()

    def andThen[R2](fn: R => R2): ComputeMagnet[R2]
  }

  case object ComputeMagnet extends Capability {

    implicit class NonPure[+R](v: () => R) extends ComputeMagnet[R] {

      override def apply(): R = v()

      override def andThen[R2](fn: R => R2): NonPure[R2] = NonPure(() => fn(v()))
    }
//    def NonPure[R](fn: => R) = new NonPure[R](() => fn)

    class Lazy[+R](fn: () => R) extends ComputeMagnet[R] {

      @transient private lazy val _v = fn()

      override def apply(): R = _v

      override def andThen[R2](fn: R => R2): Lazy[R2] = Lazy(fn(_v))
    }
    def Lazy[R](fn: => R) = new Lazy[R](() => fn)

    class Eager[+R](private val _v: R) extends ComputeMagnet[R] {

      override def apply(): R = _v

      override def andThen[R2](fn: R => R2): Eager[R2] = Eager(fn(_v))
    }

    implicit def Eager[R](v: R): Eager[R] = new Eager(v)
  }
}
