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

  }

  object Tracer {

    implicit def widen[R](self: TracerCompat[R]): TracerImpl[R] = self.asInstanceOf[TracerImpl[R]]

    case class Blackbox[R](unbox: R) extends TracerImpl[R] {}

    case class MaybeApplied[
        I,
        R
    ](
        arg: TracerCompat[I],
        fn: FnCompat[I, R]
    ) { // TODO: can this be a polymorphic function? sounds feasible in Scala 3

      lazy val resolve: TracerImpl[R] = {

        fn match {

          case _: Fn.Identity[_] =>
            arg.asInstanceOf[TracerImpl[R]]
          case _ =>
            Applied
        }
      }

      object Applied extends TracerImpl[R] with Explainable.Composite with Explainable.DecodedName {

        override def unbox: R = fn(arg.unbox)

        override def composedFrom: Seq[Explainable] = Seq(arg, fn)
      }
    }
  }

  implicit class TracerCompatOps[R](val self: TracerCompat[R]) extends Serializable {

    def map[O2](fn: R => O2): TracerImpl[O2] = {

      val result = Fn(fn).^.suspend(self)
      result
    }

    def foreach(fn: R => Unit): Unit = {
      map(fn).unbox
    }

    def flatMap[O2](fn: R => O2): O2 = {
//      val result = map(fn)
//      val v1 = result.unbox

      val _fn = Fn(fn)

      val s2: TracerImpl[O2] = _fn.^.suspend(self)
      val v2: O2 = s2.unbox

      v2
    }
  }

  implicit class TracerApply[I, O](self: TracerCompat[FnCompat[I, O]]) {

    def apply(arg: TracerCompat[I]): TracerImpl[O] = {

      val fn = self.unbox.impl

      Tracer
        .MaybeApplied(
          arg,
          fn
        )
        .resolve
    }

    lazy val suspend = apply _
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

    implicit def widen[I, O](self: FnCompat[I, O]): FnImpl[I, O] = self.asInstanceOf[FnImpl[I, O]]

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

    case class MaybeCompose[
        I,
        O1,
        O2
    ](
        f: FnCompat[I, O1],
        g: FnCompat[O1, O2]
    ) { // TODO: can this be a polymorphic function? sounds feasible in Scala 3

      lazy val resolve: FnImpl[I, O2] = {

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
  }

  type FnCompat[-I, +R] = Fn { type In >: I; type Out <: R }

  trait FnImpl[I, O] extends Fn { // most specific

    type In = I
    type Out = O

    def impl: FnImpl[I, O] = this

//    override def toString: String = self.toString // preserve reference transparency

    def cachedBy(
        cache: CacheView[I, O] = Same.ByEquality.Lookup[I, O]()
    ): Fn.Cached[I, O] = {
      new Fn.Cached[I, O](this) {

        override lazy val underlyingCache: CacheView[I, O] = cache
      }
    }

    def _andThen[O2](next: O => O2): FnImpl[I, O2] = {

      val nextFn: FnCompat[O, O2] = Fn[O, O2](next)

      val result: FnImpl[I, O2] =
        Fn.MaybeCompose[I, O, O2](this, nextFn).resolve

      result
    }

    def _compose[I1](prev: I1 => I): FnImpl[I1, O] = {

      val prevFn = Fn(prev)

      val result: FnImpl[I1, O] =
        Fn.MaybeCompose[I1, I, O](prevFn, this).resolve

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
  }

  implicit class FnRepr[I, O](val self: FnCompat[I, O]) extends (I => O) with Serializable {

    lazy val revert: FnImpl[I, O] = self.impl

    override def apply(v1: I): O = self.apply(v1)

    override def andThen[O2](g: O => O2): FnRepr[I, O2] = {

      self._andThen(g)
    }

    override def compose[A](g: A => I): FnRepr[A, O] = {
      self._compose(g)
    }
  }

  implicit def _fn[I, R](
      vanilla: I => R
  ): FnImpl[I, R] = {

    Fn[I, R](vanilla)
  }
}
