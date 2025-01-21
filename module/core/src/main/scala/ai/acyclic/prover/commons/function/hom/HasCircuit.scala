package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.Delegating
import ai.acyclic.prover.commons.cap.Capability
import ai.acyclic.prover.commons.collection.CacheMagnet
import ai.acyclic.prover.commons.function.{BuildTemplate, Traceable}
import ai.acyclic.prover.commons.function.bound.{DepDomains, Domains}
import ai.acyclic.prover.commons.multiverse.CanEqual
import ai.acyclic.prover.commons.util.SrcDefinition

import scala.language.implicitConversions

object HasCircuit {}

trait HasCircuit extends Capability.Universe {

  trait CanNormalise_Impl0 {

    implicit def _normaliseToFn1[I, O](v: CanNormalise[Fn[I, O]])(
        implicit
        _definedAt: SrcDefinition
    ): Function1View[I, O] = {
      v match {

        case vv: Function1View[_, _] => vv.asInstanceOf[Function1View[I, O]]
        case _ =>
          Function1View(v.normalise, _definedAt)
      }
    }

    private[HasCircuit] case class Function1View[I, O](
        self: Fn[I, O],
        _definedAt: SrcDefinition
    ) extends Function[I, O] {

      def function1: Function1View[I, O] = this

      final override def apply(v: I): O = self(v)

      // TODO: both of these are not narrow enough
      final override def andThen[O2](next: O => O2): Function1View[I, O2] = {

        val _next: Fn[O, O2] = Fn.at[O](next)(_definedAt)

        val result =
          Fn.Mapped[I, O, O2](self, _next)

        result
      }

      final override def compose[I1](prev: I1 => I): Function1View[I1, O] = {

        val _prev = Fn.at[I1](prev)(_definedAt)

        _prev.andThen(self)
      }

      def trace: Fn.Tracing[I, O] = Fn.Tracing(self.normalise)

      //      override def normalise: Circuit[I, O] = self.normalise
    }

    implicit def _normalisedToFn0[O](v: CanNormalise[Thunk[O]])(
        implicit
        _definedAt: SrcDefinition
    ): Function0View[O] = {
      v match {

        case vv: Function0View[_] => vv.asInstanceOf[Function0View[O]]
        case _ =>
          Function0View(v.normalise, _definedAt)
      }
    }

    private[HasCircuit] case class Function0View[O](
        self: Thunk[O],
        _definedAt: SrcDefinition
    ) extends Function0[O] {

      def function0: Function0View[O] = this

      final override def apply(): O = self(())

      def trace: Fn.Tracing[Unit, O] = Fn.Tracing(self.normalise)

      //      override def normalise: Circuit[I, O] = self.normalise

      def asLazy: Thunk.CachedLazy[O] = Thunk.CachedLazy(self)

      def asEager: Thunk.CachedEager[Thunk[O]] = Thunk.CachedEager(self)
    }
  }
  object CanNormalise extends CanNormalise_Impl0 {}

  sealed trait CanNormalise[+N <: Circuit] extends Delegating[N] {

    def normalise: N
    final def unbox: N = normalise
  }

  trait Circuit extends DepDomains with Traceable with Product with Serializable {

    def apply(arg: _I): _OK[arg.type]
  }
  object Circuit {

    implicit class _extFn[I, O](
        self: Fn[I, O]
    ) extends Serializable {

      def cached(byLookup: => CacheMagnet[I, O]): Fn.CachedLazy[I, O] = {
        Fn.CachedLazy[I, O](self)(() => byLookup)
      }

      def cached(): Fn.CachedLazy[I, O] = {
        Fn.CachedLazy[I, O](self)()
      }
    }
  }

  type DepFn[-I] =
    DepFn.K1_[I] // TODO: should be K1[I] (as refined type), but scala 2 implicit search is too weak fo this
  case object DepFn {

    type K1[-I] = Circuit { type _I >: I }
    trait K1_[-I] extends Circuit { type _I >: I }
    { // sanity
      implicitly[K1_[Int] <:< K1[Int]]
    }
  }

  type Fn[-I, +R] = Fn.K2_[I, R]
  // TODO: should be K2[I, R] (as refined type), but scala 2 implicit search is too weak fo this
  case object Fn extends BuildTemplate {

    /**
      * function with computation graph, like a lifted JAXpr
      */
    type K2[-I, +O] = DepFn.K1[I] { type _OK[T] <: O }
    trait K2_[-I, +O] extends CanNormalise[K2_[I, O]] with DepFn.K1_[I] with Domains {

      type _O <: O

      def normalise = this // bypassing EqSat, always leads to better representation
    }
    { // sanity
      implicitly[K2_[Int, String] <:< K2[Int, String]]
    }

    case class Tracing[I, O](self: Fn[I, O]) extends CanNormalise[K2_[I, O]] {

      lazy val higherOrder: Fn.Tracing[Unit, Fn[I, O]] =
        Fn.Tracing(Thunk.CachedEager(self))

      def map[O2](right: O => O2)(
          implicit
          _definedAt: SrcDefinition
      ): Tracing[I, O2] = {

        val result = self.andThen(right)

        Tracing(result)
      }

      def foreach(right: O => Unit)(
          implicit
          _definedAt: SrcDefinition
      ): Tracing[I, Unit] = {

        map(right)
      }

      def withFilter(right: O => Boolean)(
          implicit
          _definedAt: SrcDefinition
      ): Tracing[I, O] = {

        val _right = Blackbox(_definedAt)(right)

        val result =
          Fn.Filtered[I, O](self, _right)

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

      override def normalise = self.normalise
    }
//    implicit def tracing2Circuit[I, O](tracing: Tracing[I, O]): Circuit[I, O] = tracing.self

    abstract class Impl[I, O](
        implicit
        override val _definedAt: SrcDefinition
    ) extends K2_[I, O] { // most specific

      type _I = I
      type _O = O
    }

    trait Mixin

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

      val I: Identity.type = Identity
      val B: Mapped.type = Mapped
      val C: Flipped.type = Flipped
      val K: Thunk.CachedLazy.type = Thunk.CachedLazy

      val Delta: Pointwise.type = Pointwise
      val Gamma: Duplicate.type = Duplicate
    }

    trait Pure extends Mixin {}

    object Pure {

      case class Is[I, R](delegate: Fn[I, R]) extends Impl[I, R] with Pure {

        override def apply(v: I): R & delegate._OK[v.type] = delegate.apply(v)
      }
    }

    case class Identity[I]()
        extends Impl[I, I]
        with Combinator.Linear { // TOOD: this should be contravariant under DepFn

      override def apply(arg: I): I & _OK[arg.type] = arg

      case object CrossUnit extends Impl[I, (I, Unit)] with Combinator.TrivialConversion {

        override def apply(arg: I): (I, Unit) = arg -> ()
      }
    }
    def id[I]: Identity[I] = Identity[I]()

    case class Mapped[I, M, O](
        left: Fn[I, M],
        right: Fn[M, O]
    ) extends Impl[I, O]
        with Combinator.Linear {

      override def apply(arg: I): O = right(left(arg))

      override def normalise: Fn[I, O] = {
        (left, right) match {
          case (_: Identity[_], rr) => rr.normalise.asInstanceOf[Fn[I, O]]
          case (ll, _: Identity[_]) => ll.normalise.asInstanceOf[Fn[I, O]]
          case (ll, rr)             => Mapped(ll.normalise, rr.normalise)
        }
      }
    }

    case class Filtered[I, O](
        base: Fn[I, O],
        condition: Fn[O, Boolean]
    ) extends Impl[I, O]
        with Combinator.Linear {

      override def apply(arg: I): O = {
        val result = base(arg)
        if (condition(result)) result
        else throw new UnsupportedOperationException(s"Filtered out: $result")
      }
    }

    case class Flipped[I1, I2, O](
        base: Fn[(I1, I2), O]
    ) extends Impl[(I2, I1), O]
        with Combinator.Linear {

      override def apply(arg: (I2, I1)): O = {

        base.apply(arg._2 -> arg._1)
      }
    }

    case class Pointwise[I1, O1, I2, O2](
        left: Fn[I1, O1],
        right: Fn[I2, O2]
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

    case class Blackbox[I, R](
        final override val _definedAt: SrcDefinition
    )(fn: I => R)
        extends Impl[I, R] {

      override def apply(arg: I): R = {

        fn(arg)
      }
    }

    implicit def fromFunction1[I, R](fn: I => R)(
        implicit
        _definedAt: SrcDefinition
    ): Fn.Impl[I, R] = {
      fn match {
        case CanNormalise.Function1View(c, _) => c.asInstanceOf[Fn.Impl[I, R]]
        case _ =>
          Blackbox[I, R](_definedAt)(fn)
      }
    }

    implicit def fromFunction0[R](fn: () => R)(
        implicit
        _definedAt: SrcDefinition
    ): Thunk.Impl[R] = {

      fn match {
        case CanNormalise.Function0View(c, _) => c.asInstanceOf[Thunk.Impl[R]]
        case _                                => fromFunction1[Unit, R]((_: Unit) => fn())
      }
    }

    trait Cached extends Pure

    // TODO: make a dependent class, also in Thunk
    final case class CachedLazy[I, R](backbone: Fn[I, R])(
        getLookup: () => CacheMagnet[I, R] = () => CanEqual.Native.Lookup[I, R]()
    ) extends Impl[I, R]
        with Cached {

      lazy val lookup: CacheMagnet[I, R] = getLookup()

      def apply(key: I): R = {
        lookup.getOrElseUpdateOnce(key) {

          val value = backbone(key)
          value
        }
      }

      def getExisting(arg: I): Option[R] = {
        lookup
          .get(arg)
      }
    }

    override protected type BuildTarget[I, O] = Fn.Impl[I, O]

    protected def build[I, O](fn: I => O)(
        implicit
        _definedAt: SrcDefinition
    ): BuildTarget[I, O] = {

      Fn.fromFunction1(fn)(_definedAt)
    }

    case class BuildDomains[I, O]() extends IBuildDomains[I, O] {

      type _Lemma = Fn[I, O]
      type _Impl = Fn.Impl[I, O]
      type _Native = (I => O)
    }
    def refine[i, o]: BuildDomains[i, o] = BuildDomains()
  }

  type Thunk[+O] = Thunk.K[O]

  object Thunk {

    type K[+O] = Fn[Unit, O]
    type Impl[O] = Fn.Impl[Unit, O]

    type Const[O] = Impl[O] & Fn.Pure
    sealed trait Const_[O] extends Impl[O] with Fn.Pure

    type Cached[O] = Impl[O] & Fn.Cached
    sealed trait Cached_[O] extends Const_[O] with Fn.Cached {

      protected def value: O

      override def apply(arg: Unit): O = value
    }

    final case class CachedLazy[O](gen: Thunk[O]) extends Cached_[O] {

      // equivalent to CachedLazy[Unit, O], but much faster
      @transient protected lazy val value: O = gen(())
    }

    final case class CachedEager[O](value: O) extends Cached_[O] {}

  }
}
