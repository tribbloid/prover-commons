package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.collection.CacheView
import ai.acyclic.prover.commons.same.Same
import ai.acyclic.prover.commons.util.{SrcExplainable, SrcPosition}

import scala.language.implicitConversions

trait HasFn {

//  sealed trait Tracer extends Traced {
//
//    def unbox: Out
//
//    type In = Unit
//  }

  object Tracer {

//    implicit def widen[R](self: TracerCompat[R]): TracerImpl[R] = self.asInstanceOf[TracerImpl[R]]

//    case class Blackbox[R](unbox: R) extends TracerImpl[R] {}

//    case class MaybeApplied[
//        I,
//        R
//    ](
//        arg: TracerCompat[I],
//        fn: FnCompat[I, R]
//    ) { // TODO: can this be a polymorphic function? sounds feasible in Scala 3
//
//      lazy val suspend: TracerImpl[R] = {
//
//        fn match {
//
//          case _: Fn.Identity[_] =>
//            arg.asInstanceOf[TracerImpl[R]]
//          case _ =>
//            Suspend
//        }
//      }
//
//      lazy val applied: TracerImpl[R] = {
//
//        fn match {
//
//          case _: Fn.Identity[_] =>
//            arg.asInstanceOf[TracerImpl[R]]
//          case _ =>
//            Applied
//        }
//      }

//      sealed trait Suspend extends TracerImpl[R] with SrcExplainable.Composite with SrcExplainable.DecodedName {
//
//        override def composedFrom: Seq[SrcExplainable] = Seq(arg, fn)
//      }
//
//      object Suspend extends Suspend {
//
//        override def unbox: R = fn(arg.unbox)
//
//        def applied: Applied.type = Applied
//      }
//
//      object Applied extends Suspend {
//
//        override val unbox: R = fn(arg.unbox)
//      }
    }

  }

//  type TracerCompat[+R] = Tracer { type Out <: R }

//  trait TracerImpl[R] extends Tracer {
//
//    import TracerImpl._
//
//    final type Out = R
//
//    def + = ContinuationView(this)
//  }

//  object TracerImpl {
//
//    implicit class ContinuationView[R](self: TracerImpl[R])(
//        implicit
//        val _definedAt: SrcPosition
//    ) {
//
//      def map[O2](fn: R => O2): TracerImpl[O2] = {
//
//        val result = Fn(fn).^.suspend(self)
//        result
//      }
//
//      def foreach(fn: R => Unit): Unit = {
//        map(fn).unbox
//      }
//
//      def flatMap[O2](fn: R => O2): O2 = {
//        //      val result = map(fn)
//        //      val v1 = result.unbox
//
//        val _fn = Fn(fn)
//
//        val s2: TracerImpl[O2] = _fn.^.apply(self)
//        val v2: O2 = s2.unbox
//
//        v2
//      }
//    }
//  }

//  implicit class TracerApply[I, O](self: TracerCompat[FnCompat[I, O]]) {
//
//    private def maybe(arg: TracerCompat[I]) = {
//
//      val fn = self.unbox
//
//      Tracer
//        .MaybeApplied(
//          arg,
//          fn
//        )
//    }
//
//    def suspend(arg: TracerCompat[I]): TracerImpl[O] = {
//      maybe(arg).suspend
//    }
//
//    def apply(arg: TracerCompat[I]): TracerImpl[O] = {
//      maybe(arg).applied
//    }
//  }

  // TODO: cleanup for effect
//  implicit def _unbox[I](v: TracerCompat[I]): I = v.unbox

  sealed trait Fn[-I, +O ] extends Function[I, O]

  object Fn {

    trait FnImpl[I, O] extends Fn[I, O] { // most specific

      type IMax = I
      type OMin = O

      def cachedBy(
                    cache: CacheView[I, O] = Same.ByEquals.Lookup[I, O]()
                  ): Fn.Cached[I, O] = {
        new Fn.Cached[I, O](this) {

          override lazy val underlyingCache: CacheView[I, O] = cache
        }
      }

      final def andThen[O2](next: O => O2)(
        implicit
        _definedAt: SrcPosition
      ): FnImpl[I, O2] = {

        val result =
          Fn.MaybeCompose[I, O, O2](this, next).resolved

        result
      }

      final def compose[I1](prev: I1 => I)(
        implicit
        _definedAt: SrcPosition
      ): FnImpl[I1, O] = {

        val result =
          Fn.MaybeCompose[I1, I, O](prev, this).resolved

        result
      }

      def out: FnImpl.Continuation[I, O] = FnImpl.Continuation(this)

      def asScala = AsScalaFunction(this)
    }

//    implicit def widen[I, O](self: FnCompat[I, O]): FnImpl[I, O] = self.asInstanceOf[FnImpl[I, O]]

//    implicit class TracingView[T <: Fn](self: T) {
//
//      def ^ : Tracer.Blackbox[T] =
//        Tracer.Blackbox(self)
//    }

    case class Identity[I]() extends FnImpl[I, I] {

      override def apply(arg: I): I = arg
    }

    def id[I]: Identity[I] = Identity[I]()

    class Blackbox[I, R](
        val fn: I => R,
        override val _definedAt: SrcPosition
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
        with SrcExplainable.Composite1 {

      lazy val underlyingCache: CacheView[I, R] = Same.Native.Lookup[I, R]()

      final def apply(key: I): R = {
        underlyingCache.getOrElseUpdateOnce(key)(backbone(key))
      }

      final def getExisting(arg: I): Option[R] = {
        underlyingCache
          .get(arg)
      }
    }

    case class PreBuild[I, O](scalaFunction: I => O) {

      lazy val notIdentity: Option[I => O] = {

        scalaFunction match {
          case _: Fn.Identity[_] =>
            None
          case _ =>
            Some(scalaFunction)
        }
      }

      def traced(
          implicit
          _definedAt: SrcPosition
      ): Option[FnImpl[I, O]] = {

        notIdentity.map(Fn.build.apply)
      }
    }

    trait PartiallyTraced[I, O] extends (I => O) {

      def trace(
          implicit
          _definedAt: SrcPosition
      ): FnImpl[I, O]
    }

    case class MaybeCompose[
        I,
        O1,
        O2
    ](
        f: I => O1,
        g: O1 => O2
    ) { // TODO: can this be a polymorphic function? sounds feasible in Scala 3

      trait ComposeLike extends (I => O2) with SrcExplainable.Composite with SrcExplainable.DecodedName {

        override def apply(v1: I): O2 = {

          g(f(v1))
        }
      }

      object PartiallyResolved extends PartiallyTraced[I, O2] with ComposeLike {

        override def trace(
            implicit
            _definedAt: SrcPosition
        ): Compose = {

          Compose()
        }

        override lazy val composedFrom: Seq[SrcExplainable] = {

          Seq(f, g).collect { case x: SrcExplainable => x }
        }
      }

      case class Compose()(
          implicit
          override val _definedAt: SrcPosition
      ) extends FnImpl[I, O2]
          with ComposeLike {

        lazy val fTraced = Fn(f)
        lazy val gTraced = Fn(g)

        override lazy val composedFrom: Seq[SrcExplainable] = Seq(fTraced, gTraced)
      }

      lazy val partiallyResolved = {

        (f, g) match {

          case (_: Fn.Identity[_], _g) =>
            _g.asInstanceOf[I => O2]
          case (_f, _: Fn.Identity[_]) =>
            _f.asInstanceOf[I => O2]
          case _ =>
            PartiallyResolved
        }
      }

      def resolved(
          implicit
          _definedAt: SrcPosition
      ) = {

        Fn(partiallyResolved)
      }

//      @transient lazy val foldIdentity: (Option[I => O1], Option[O1 => O2]) = {
//
//        (f, g) match {
//
//          case (_: Fn.Identity[_], _g) =>
//            None -> Some(_g)
//          case (_f, _: Fn.Identity[_]) =>
//            Some(_f) -> None
//          case _ =>
//            Some(f) -> Some(g)
//        }
//      }

//      object Raw {
//
//        trait Compose extends (I => O2) {
//
//          override def apply(arg: I): O2 = {
//            val o1: O1 = f.apply(arg)
//            g.apply(o1)
//          }
//        }
//        object Compose extends Compose
//
//        lazy val effective = {
//          foldIdentity
//
//        }
//
//      }
//
//      case class Resolve()(
//          implicit
//          val _definedAt: SrcPosition
//      ) {}
//
//      lazy val resolve: FnImpl[I, O2] = {
//
//        (f, g) match {
//
//          case (_: Fn.Identity[_], _g) =>
//            _g.asInstanceOf[FnImpl[I, O2]]
//          case (_f, _: Fn.Identity[_]) =>
//            _f.asInstanceOf[FnImpl[I, O2]]
//          case _ =>
//            RawCompose
//        }
//      }
    }
  }

  implicit class _BuildFn(self: Fn.type) extends FnBuilder {

    def build = this

    override type =>>[i, o] = FnImpl[i, o]

    override def define[I, O](vanilla: I => O)(
        implicit
        _definedAt: SrcPosition
    ): I =>> O = {

      vanilla match {
        case already: FnImpl[I, O] =>
          already
        case partial: Fn.PartiallyTraced[I, O] =>
          partial.trace
        case _ =>
          new Fn.Blackbox[I, O](vanilla, _definedAt)
      }
    }
  }

//  type FnCompat[-I, +O] = FnImpl[_ >: I, _ <: O]


  object FnImpl {

    case class Continuation[I, O](self: FnImpl[I, O]) {

      def + = ContinuationView(this)
    }

//    implicit class ContinuationView[I, O](continuation: Continuation[I, O])(
//        implicit
//        val _definedAt: SrcPosition
//    ) {
//
//      lazy val self = continuation.self
//
//      def map[O2](next: O => O2): FnImpl[I, O2] = {
//
//        val result = self.andThen(next)
//
//        result
//      }
//
//      def foreach(
//          implicit
//          _definedAt: SrcPosition
//      ) = map[Unit] _
//
//      def flatMap[T](fn: O => T)(
//          implicit
//          _definedAt: SrcPosition
//      ): FnImpl[I, T] = {
//        map(fn)
//      }
//    }
  }

  // Fn ==> scala function
//  implicit class FnRepr[I, O](val self: FnCompat[I, O]) extends (I => O) {
//
//    lazy val revert: FnImpl[I, O] = self.impl
//
//    override def apply(v1: I): O = self.apply(v1)
//
//    override def andThen[O2](g: O => O2): FnRepr[I, O2] = {
//
//      self._andThen(g)
//    }
//
//    override def compose[A](g: A => I): FnRepr[A, O] = {
//      self._compose(g)
//    }
//  }

  // TODO: bidirectional conversion should be avoided, not strongly normalising
  // scala function ==> Fn
//  implicit def _fn[I, R](
//      vanilla: I => R
//  )(
//      implicit
//      _definedAt: SrcPosition
//  ): FnImpl[I, R] = {
//
//    Fn[I, R](vanilla)
//  }
}
