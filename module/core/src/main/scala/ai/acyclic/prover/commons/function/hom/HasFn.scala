package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.collection.CacheView
import ai.acyclic.prover.commons.debug.Debug.CallStackRef
import ai.acyclic.prover.commons.same.Same

import scala.language.implicitConversions

trait HasFn {

  type TracerCompat[+R] = Tracer { type Out <: R }

  trait TracerImpl[R] extends Tracer {

    final type Out = R
  }

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

  implicit class TracerCompatOps[R](val self: TracerCompat[R]) extends Serializable {

    def map[O2](fn: R => O2): TracerImpl[O2] = {

      Fn(fn).^.trace_apply(self)
    }

    def foreach(fn: R => Unit): Unit = {
      map(fn).unbox
    }

    def flatMap[O2](fn: R => O2): O2 = {
      ???
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

  sealed trait Fn extends Tracing with Explainable {

    def ^ : Tracer.Blackbox[Fn.this.type] =
      Tracer.Blackbox[Fn.this.type](Fn.this)
  }

  object Fn extends Builder {

    override type =>>[i, o] = FnImpl[i, o]

    override def define[I, O](vanilla: I => O): I =>> O = {

      vanilla match {
        case ops: FnRepr[I, O] =>
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

  case class MaybeCompose[
      I,
      O1,
      O2
  ](
      f: FnCompat[I, O1],
      g: FnCompat[O1, O2]
  ) {

    lazy val reduce: FnImpl[I, O2] = {

      (f, g) match {

        case (_: Fn.Identity[_], _g) =>
          _g.asInstanceOf[FnImpl[I, O2]]
        case (_f, _: Fn.Identity[_]) =>
          _f.asInstanceOf[FnImpl[I, O2]]
        case _ =>
          Compose
      }
    }

    object Compose extends FnImpl[I, O2] with Explainable.Composite with Explainable.DecodedName {

      override def apply(arg: I): O2 = {
        val o1: O1 = f.apply(arg)
        g.apply(o1)
      }

      override def composedFrom: Seq[Explainable] = Seq(f, g)
    }
  }

  implicit class FnRepr[I, O](val self: FnCompat[I, O]) extends (I => O) with Serializable {

    def _andThen[O2](next: O => O2): FnImpl[I, O2] = {

      val nextFn: FnCompat[O, O2] = Fn[O, O2](next)

      val result: FnImpl[I, O2] =
        MaybeCompose[I, O, O2](self, nextFn).reduce

      result
    }

    def _compose[I1](prev: I1 => I): FnImpl[I1, O] = {

      val prevFn = Fn(prev)

      val result: FnImpl[I1, O] =
        MaybeCompose[I1, I, O](prevFn, self).reduce

      result
    }

    case class Continuation private () {

      def map[O2](next: O => O2): FnImpl[I, O2] = {

        val result = _andThen(next): FnImpl[I, O2]

        result
      }

      def foreach = map[Unit] _

      def flatMap[T](fn: O => T): FnImpl[I, T] = {
        map(fn)
      }
    }

    lazy val out = Continuation()

    lazy val revert: FnImpl[I, O] = self.widen[I, O]

    override def apply(v1: I): O = self.apply(v1)

    override def andThen[O2](g: O => O2): FnRepr[I, O2] = {

      _andThen(g)
    }

    override def compose[A](g: A => I): FnRepr[A, O] = {
      _compose(g)
    }
  }

  implicit def _fn[I, R](
      vanilla: I => R
  ): FnImpl[I, R] = {

    Fn[I, R](vanilla)
  }

  // shortcuts

}
