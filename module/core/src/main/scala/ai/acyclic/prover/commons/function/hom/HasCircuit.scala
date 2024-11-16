package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.cap.Capability
import ai.acyclic.prover.commons.collection.LookupMagnet
import ai.acyclic.prover.commons.function.Traceable
import ai.acyclic.prover.commons.function.Traceable.BySrc
import ai.acyclic.prover.commons.same.Same
import ai.acyclic.prover.commons.util.SrcPosition

import scala.language.implicitConversions

object HasCircuit {}

trait HasCircuit extends Capability.Universe {

  trait CanNormalise_Impl0 {

    implicit def asFunction1[I, O](v: CanNormalise[I, O])(
        implicit
        _definedAt: SrcPosition
    ): Circuit.Function1View[I, O] = {
      v match {

        case vv: Circuit.Function1View[_, _] => vv.asInstanceOf[Circuit.Function1View[I, O]]
        case _ =>
          Circuit.Function1View(v.normalise, _definedAt)
      }
    }

    implicit def asFunction0[O](v: CanNormalise[Unit, O])(
        implicit
        _definedAt: SrcPosition
    ): Thunk.Function0View[O] = {
      v match {

        case vv: Thunk.Function0View[_] => vv.asInstanceOf[Thunk.Function0View[O]]
        case _ =>
          Thunk.Function0View(v.normalise, _definedAt)
      }
    }
  }
  object CanNormalise extends CanNormalise_Impl0 {

    implicit def asCircuit[I, O](v: CanNormalise[I, O]): Circuit[I, O] = v.normalise

  }

  sealed trait CanNormalise[-I, +O] {

    def normalise: Circuit[I, O]
  }

  type Circuit[-I, +R] = Circuit.Fn[I, R]

  object Circuit extends Serializable {

    /**
      * function with computation graph, like a lifted JAXpr
      */
    trait Fn[-I, +O] extends CanNormalise[I, O] with Traceable with Serializable {

      def apply(arg: I): O

      def normalise: Circuit[I, O] = this // bypassing EqSat, always leads to better representation
    }

    sealed trait Theorem[-I, +O] extends Fn[I, O] {

      //    type In >: I
      type Out <: O

      def apply(arg: I): Out
    }

    implicit class _extension[I, O](
        self: Circuit[I, O]
    ) {

      def cached(
          byLookup: LookupMagnet[I, O] = Same.Native.Lookup[I, O]()
      ): Circuit.CachedLazy[I, O] = {
        new Circuit.CachedLazy[I, O](self) {

          override lazy val underlyingCache: LookupMagnet[I, O] = byLookup
        }
      }
    }

    private[HasCircuit] case class Function1View[I, O](
        self: Circuit[I, O],
        _definedAt: SrcPosition
    ) extends Function[I, O] {

      def function1: Function1View[I, O] = this

      final override def apply(v: I): O = self(v)

      // TODO: both of these are not narrow enough
      final override def andThen[O2](next: O => O2): Function1View[I, O2] = {

        val _next = Circuit.define(next)(_definedAt)

        val result =
          Circuit.Mapped[I, O, O2](self, _next)

        result
      }

      final override def compose[I1](prev: I1 => I): Function1View[I1, O] = {

        val _prev = Circuit.define(prev)(_definedAt)

        _prev.andThen(self)
      }

      def trace: Circuit.Tracing[I, O] = Circuit.Tracing(self.normalise)

//      override def normalise: Circuit[I, O] = self.normalise
    }

    case class Tracing[I, O](self: Circuit[I, O]) extends CanNormalise[I, O] {
      // TODO: this can be fold into Circuit?

      lazy val higher: Circuit.Tracing[Unit, Circuit[I, O]] =
        Circuit.Tracing(Thunk.Eager(self))

      def map[O2](right: O => O2)(
          implicit
          _definedAt: SrcPosition
      ): Tracing[I, O2] = {

        val result = self.andThen(right)

        Tracing(result)
      }

      def foreach(right: O => Unit)(
          implicit
          _definedAt: SrcPosition
      ): Tracing[I, Unit] = {

        map(right)
      }

      def withFilter(right: O => Boolean)(
          implicit
          _definedAt: SrcPosition
      ): Tracing[I, O] = {

        val _right = Blackbox()(right)

        val result =
          Circuit.Filtered[I, O](self, _right)

        Tracing(result)
      }

      def ><[I2, O2](right: Tracing[I2, O2]): Tracing[(I, I2), (O, O2)] = {

        val result = Pointwise(self, right.self)

        result.trace
      }

      def -<[O2](right: Tracing[I, O2]): Tracing[I, (O, O2)] = {

        val first = Duplicate[I]()
        val second = Pointwise(self, right.self)

        first.andThen(second).self.trace
      }

      // flatMap is undefined, there are several options, see dottyspike ForComprehension spike for details

      override def normalise: Circuit[I, O] = self.normalise
    }
//    implicit def tracing2Circuit[I, O](tracing: Tracing[I, O]): Circuit[I, O] = tracing.self

    trait Mixin

    trait Impl[I, O] extends Mixin with Theorem[I, O] { // most specific

      final type In = I
      final type Out = O
    }

    trait Pure extends Mixin {}

    object Pure {

      case class Is[I, R](self: Circuit[I, R]) extends Impl[I, R] with Pure {
        override def apply(arg: I): R = self.apply(arg)
      }
    }

    sealed trait Combinator extends Mixin
    object Combinator {

      trait Affine extends Combinator
      trait Linear extends Affine
//      trait NonLinear extends Combinator

      trait TrivialConversion extends Linear
      // for conversion between extensionally equal types (but cannot be represented in the current type system)

      // TODO: not all are defined, will add more in the following order:
      //  - B/C: used in autograd
      //  - delta/gamma: used in interaction combinators
      //  - S: used in STLC
      //  - Y: don't know what is it for

      val I = Identity
      val B = Mapped
      val C = Flipped
      val K = Thunk.Lazy

      val Delta = Pointwise
      val Gamma = Duplicate
    }

    case class Identity[I]() extends Impl[I, I] with Combinator.Linear {

      override def apply(arg: I): I = arg

      case object CrossUnit extends Impl[I, (I, Unit)] with Combinator.TrivialConversion {

        override def apply(arg: I): (I, Unit) = arg -> ()
      }
    }
    def id[I]: Identity[I] = Identity[I]()

    case class Mapped[I, M, O](
        left: Circuit[I, M],
        right: Circuit[M, O]
    ) extends Impl[I, O]
        with Combinator.Linear {

      override def apply(arg: I): O = right(left(arg))

      override def normalise: Circuit[I, O] = {
        (left, right) match {
          case (_: Identity[_], rr) => rr.normalise.asInstanceOf[Circuit[I, O]]
          case (ll, _: Identity[_]) => ll.normalise.asInstanceOf[Circuit[I, O]]
          case (ll, rr)             => Mapped(ll.normalise, rr.normalise)
        }
      }
    }

    case class Filtered[I, O](
        base: Circuit[I, O],
        condition: Circuit[O, Boolean]
    ) extends Impl[I, O]
        with Combinator.Linear {

      override def apply(arg: I): O = {
        val result = base(arg)
        if (condition(result)) result
        else throw new UnsupportedOperationException(s"Filtered out: $result")
      }
    }

    case class Flipped[I1, I2, O](
        base: Circuit[(I1, I2), O]
    ) extends Impl[(I2, I1), O]
        with Combinator.Linear {

      override def apply(arg: (I2, I1)): O = {

        base.apply(arg._2 -> arg._1)
      }
    }

    case class Pointwise[I1, O1, I2, O2](
        left: Circuit[I1, O1],
        right: Circuit[I2, O2]
    ) extends Impl[(I1, I2), (O1, O2)]
        with Combinator.Linear {

      override def apply(arg: (I1, I2)): (O1, O2) = {
        val lo = left(arg._1)
        val ro = right(arg._2)

        lo -> ro
      }
    }

    case class Duplicate[I]() extends Impl[I, (I, I)] {

      override def apply(arg: I): (I, I) = arg -> arg
    }

    case class DiscardRight[I1, I2](
    ) extends Impl[(I1, I2), I1]
        with Combinator.Affine {

      override def apply(arg: (I1, I2)): I1 = arg._1
    }

    // TODO: remove, equals Pointwise + DiscardRight
//    case class AbsorbLeft[I, O1, O2](
//        left: Thunk[O1],
//        right: Circuit[I, O2]
//    ) extends Impl[I, (O1, O2)]
//        with Combinator.Linear {
//
//      override def apply(arg: I): (O1, O2) = {
//
//        val lo = left(())
//        val ro = right(arg)
//
//        lo -> ro
//      }
//    }

    // TODO: remove, equals Duplicate + Pointwise
//    case class Fork[I, O1, O2](
//        left: Circuit[I, O1],
//        right: Circuit[I, O2]
//    ) extends Impl[I, (O1, O2)]
//        with Combinator {
//
//      override def apply(arg: I): (O1, O2) = {
//
//        val lo = left(arg)
//        val ro = right(arg)
//
//        lo -> ro
//      }
//    }

    // there is no absorb right

    case class Blackbox[I, R]()(fn: I => R)(
        implicit
        final override val _definedAt: SrcPosition
    ) extends Impl[I, R]
        with BySrc {

      override def apply(arg: I): R = fn(arg)
    }

    implicit def fromFunction1[I, R](fn: I => R)(
        implicit
        _definedAt: SrcPosition
    ): Circuit[I, R] = {
      fn match {
        case Function1View(c, _) => c
        case _ =>
          Blackbox[I, R]()(fn)
      }
    }

    implicit def fromFunction0[R](fn: () => R)(
        implicit
        _definedAt: SrcPosition
    ): Circuit[Unit, R] = {

      fn match {
        case Thunk.Function0View(c, _) => c
        case _                         => fromFunction1[Unit, R]((_: Unit) => fn())
      }
    }

    trait Cached extends Pure

    case class CachedLazy[I, R](
        backbone: Circuit[I, R]
    ) extends Impl[I, R]
        with Cached {

      lazy val underlyingCache: LookupMagnet[I, R] = Same.Native.Lookup[I, R]()

      final def apply(key: I): R = {
        underlyingCache.getOrElseUpdateOnce(key) {

          val value = backbone(key)
          value
        }
      }

      final def getExisting(arg: I): Option[R] = {
        underlyingCache
          .get(arg)
      }
    }

  }

  implicit class _CircuitBuilder(self: Circuit.type) extends FromFunctionBuilder {

    def build: this.type = this

    override type Target[i, o] = Circuit.Impl[i, o]

    override def define[I, O](vanilla: I => O)(
        implicit
        _definedAt: SrcPosition
    ): I Target O = {

      vanilla match {
        case fnView: self.Function1View[_, _] => fnView.self.asInstanceOf[Circuit.Impl[I, O]]
        case _                                => self.Blackbox()(vanilla)(_definedAt)
      }
    }
  }

  type Thunk[+O] = Circuit[Unit, O]

  object Thunk {

    type Impl[O] = Circuit.Impl[Unit, O]

    private def __sanity[O]() = {

      implicitly[Impl[O] <:< Thunk[O]]
    }

    type Const[O] = Impl[O] & Circuit.Pure
    sealed trait Const_[O] extends Impl[O] with Circuit.Pure

    type Cached[O] = Impl[O] & Circuit.Cached
    sealed trait Cached_[O] extends Const_[O] with Circuit.Cached {

      protected def value: O

      override def apply(arg: Unit): O = value
    }

    case class Lazy[O](gen: () => O)(
        implicit
        final override val _definedAt: SrcPosition
    ) extends Cached_[O]
        with Traceable.BySrc {

      // equivalent to CachedLazy[Unit, O], but much faster
      protected lazy val value = gen()

    }

    case class Eager[O](value: O) extends Cached_[O] {}

    private[HasCircuit] case class Function0View[O](
        self: Thunk[O],
        _definedAt: SrcPosition
    ) extends Function0[O] {

      def function0: Function0View[O] = this

      final override def apply(): O = self(())

      def trace: Circuit.Tracing[Unit, O] = Circuit.Tracing(self.normalise)

      //      override def normalise: Circuit[I, O] = self.normalise
    }
  }
}
