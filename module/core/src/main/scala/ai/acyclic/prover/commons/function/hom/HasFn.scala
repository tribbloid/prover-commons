package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.collection.CacheView
import ai.acyclic.prover.commons.debug.Debug.CallStackRef
import ai.acyclic.prover.commons.same.Same

import scala.language.implicitConversions

trait HasFn {

  sealed trait Tracer extends Tracing {

    def unbox: Out

    type In = Unit

    override def apply(arg: Unit): Out = unbox

    def widen[T >: Out]: TracerImpl[T] = this.asInstanceOf[TracerImpl[T]]
  }

  object Tracer {

    case class Blackbox[R](unbox: R) extends TracerImpl[R] {}

    case class Applied[I, R](
        fn: FnCompat[I, R],
        arg: TracerCompat[I]
    ) extends TracerImpl[R] {

      override def unbox: R = fn(arg.unbox)
    }
  }

  implicit class TracerApply[I, O](self: TracerCompat[FnCompat[I, O]]) {

    def apply(arg: TracerCompat[I]): Tracer.Applied[I, O] = {

      Tracer.Applied(
        self.unbox,
        arg
      )
    }

    def trace_apply(arg: TracerCompat[I]): Tracer.Applied[I, O] = apply(arg)
  }

  implicit def _unbox[I](v: TracerCompat[I]): I = v.unbox

  type TracerCompat[+R] = Tracer { type Out <: R }

  trait TracerImpl[R] extends Tracer {

    final type Out = R
  }

  sealed trait Fn extends Tracing with Explainable {
    // thin wrapper of a tracer

    def ^ : Tracer.Blackbox[Fn.this.type] =
      Tracer.Blackbox[Fn.this.type](Fn.this)
  }

  object Fn extends Builder {

    override type =>>[i, o] = FnImpl[i, o]

    override def define[I, O](vanilla: I => O): I =>> O = {

      vanilla match {
        case ops: FnReprLike[I, O] =>
          ops.revert
        case _ =>
          new Blackbox[I, O](vanilla, implicitly[CallStackRef])
      }
    }

    case class Identity[I]() extends FnImpl[I, I] {

      override def apply(arg: I): I = arg
    }

    def id[I]: Identity[I] = Identity[I]()

    class Blackbox[I, R](
        val fn: I => R,
        override val _definedAt: CallStackRef = definedHere
    ) extends FnImpl[I, R] {

      override def apply(arg: I): R = fn(arg)
    }

    trait Pure[I, R] extends FnImpl[I, R] {}

    object Pure {
      // user need to manually verify purity

      class ^[I, R](self: FnCompat[I, R]) extends Pure[I, R] {
        override def apply(arg: I): R = self.apply(arg)
      }
    }

    class Cached[I, R](
        val backbone: FnCompat[I, R]
    ) extends Pure[I, R]
        with Explainable.Composite1 {

      lazy val underlyingCache: CacheView[I, R] = Same.ByEquality.Lookup[I, R]()

      final def apply(key: I): R = {
        underlyingCache.getOrElseUpdateOnce(key)(backbone(key))
      }

      final def getExisting(arg: I): Option[R] = {
        underlyingCache
          .get(arg)
      }
    }
  }

  type FnCompat[-I, +R] = Fn { type In >: I; type Out <: R }

  trait FnImpl[I, R] extends Fn { // most specific

    type In = I
    type Out = R
  }

  implicit class FnOps[I, O](val self: FnCompat[I, O]) extends Serializable {

    override def toString: String = self.toString // preserve reference transparency

    def widen[i <: I, o >: O]: FnImpl[i, o] = self.asInstanceOf[FnImpl[i, o]]

    def cachedBy(
        cache: CacheView[I, O] = Same.ByEquality.Lookup[I, O]()
    ): Fn.Cached[I, O] = {
      new Fn.Cached[I, O](self) {

        override lazy val underlyingCache: CacheView[I, O] = cache
      }
    }
  }

  trait FnReprLike[I, O] extends Serializable with (I => O) {

    def revert: FnImpl[I, O]
  }

  implicit def fromScalaFn[I, R](
      vanilla: I => R
  ): FnImpl[I, R] = {
//    implicit val definedAt: CallStackRef =

    Fn[I, R](vanilla)
  }

  // shortcuts

}
