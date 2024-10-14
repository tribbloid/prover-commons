package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.cap.Capabilities
import ai.acyclic.prover.commons.collection.CacheView
import ai.acyclic.prover.commons.function.Traceable
import ai.acyclic.prover.commons.function.Traceable.BySrc
import ai.acyclic.prover.commons.same.Same
import ai.acyclic.prover.commons.util.SrcPosition

import scala.language.implicitConversions

object HasCircuit extends Capabilities {}

trait HasCircuit {

  /**
    * function with computation graph, like a lifted JAXpr
    */
  sealed trait Circuit[-I, +O] extends Traceable with Serializable {

    def apply(arg: I): O
  }

  object Circuit {

    implicit class InvariantView[I, O](
        self: Circuit[I, O]
    ) {

      lazy val trace: Circuit.Tracing[I, O] = Circuit.Tracing(self)

    }

    implicit class FunctionView[-I, +O](val self: Circuit[I, O])(
        implicit
        _definedAt: SrcPosition
    ) extends Function[I, O] {

      final override def apply(v: I) = self(v)

      // TODO: both of these are not narrow enough
      final override def andThen[O2](next: O => O2): FunctionView[I, O2] = {

        val _next = Blackbox(next)(_definedAt)

        val result =
          Circuit.Mapped[I, O, O2](self, _next)

        result
      }

      final override def compose[I1](prev: I1 => I): FunctionView[I1, O] = {

        val _prev = Blackbox(prev)(_definedAt)

        _prev.andThen(self)
      }

    }
//    implicit def circuit2FunctionView[I, O](circuit: Circuit[I, O]): FunctionView[I, O] = FunctionView(circuit)
    implicit def functionView2Circuit[I, O](fn: FunctionView[I, O]): Circuit[I, O] = fn.self

    case class Tracing[I, O](self: Circuit[I, O])(
        implicit
        _definedAt: SrcPosition
    ) {
      // TODO: this can be fold into Circuit?

      lazy val higher: Circuit.Tracing[Unit, Circuit[I, O]] =
        Circuit.Tracing(Thunk.Eager(self))

      def map[O2](right: O => O2)(): Tracing[I, O2] = {

        val result = self.andThen(right)

        Tracing(result)
      }

      def foreach(right: O => Unit): Tracing[I, Unit] = {

        map(right)
      }

      def withFilter(right: O => Boolean): Tracing[I, O] = {

        val _right = Blackbox(right)(_definedAt)

        val result =
          Circuit.WithFilter[I, O](self, _right)

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
    }
    implicit def tracing2Circuit[I, O](tracing: Tracing[I, O]): Circuit[I, O] = tracing.self

    trait Mixin

    trait Impl[I, O] extends Mixin with Circuit[I, O] { // most specific

      type IMax = I
      type OMin = O

      def cachedBy(
          cache: CacheView[I, O] = Same.Native.Lookup[I, O]()
      ): Circuit.CachedLazy[I, O] = {
        new Circuit.CachedLazy[I, O](this) {

          override lazy val underlyingCache: CacheView[I, O] = cache
        }
      }
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
    }
    def id[I]: Identity[I] = Identity[I]()

    case class Mapped[I, M, O](
        left: Circuit[I, M],
        right: Circuit[M, O]
    ) extends Impl[I, O]
        with Combinator.Linear {

      override def apply(arg: I): O = right(left(arg))
    }

    case class WithFilter[I, O](
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

    case class Blackbox[I, R](fn: I => R)(
        implicit
        final override val _definedAt: SrcPosition
    ) extends Impl[I, R]
        with BySrc {

      override def apply(arg: I): R = fn(arg)
    }

    case class CachedLazy[I, R](
        backbone: Circuit[I, R]
    ) extends Impl[I, R]
        with Pure {

      lazy val underlyingCache: CacheView[I, R] = Same.Native.Lookup[I, R]()

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

  implicit class _CircuitBuilder(self: Circuit.type) extends FnBuilder {

    def build = this

    override type =>>[i, o] = Circuit.Impl[i, o]

    override def define[I, O](vanilla: I => O)(
        implicit
        _definedAt: SrcPosition
    ): I =>> O = {

      vanilla match {
        case already: Circuit.Impl[I, O] =>
          already
//        case partial: Circuit.PartiallyTraced[I, O] =>
//          partial.trace
        case _ =>
          new Circuit.Blackbox[I, O](vanilla)(_definedAt)
      }
    }
  }

  type Thunk[+O] = Circuit[Unit, O]

  object Thunk {

    type Impl[O] = Circuit.Impl[Unit, O]

    private def __sanity[O]() = {

      implicitly[Impl[O] <:< Thunk[O]]
    }

    type Const[O] = Impl[O] with Circuit.Pure
    sealed trait Const_[O] extends Impl[O] with Circuit.Pure

    case class Lazy[O](gen: () => O) extends Const_[O] {

      override def apply(arg: Unit): O = gen()
    }

    case class Eager[O](value: O) extends Const_[O] {

      override def apply(arg: Unit): O = value
    }
  }
}
