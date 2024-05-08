package ai.acyclic.prover.commons.function.api

import ai.acyclic.prover.commons.collection.CacheView
import ai.acyclic.prover.commons.debug.Debug.CallStackRef
import ai.acyclic.prover.commons.same.Same

import scala.language.implicitConversions

trait HasFn {

  protected[function] def definedHere: CallStackRef = CallStackRef
    .below(condition = { v =>
      v.isDefinedAtClasses(classOf[HasFn]) || v.isArgDefault
    })
    .below(1)

  /**
    * will be Tuple-like in the future
    */
  type IUB // TODO: should be "Domain"

  type Arg1[T] <: IUB
  def arg1[T](v: T): Arg1[T]

  type _Unit <: IUB
  val _unit: _Unit // terminal object in cat theory, can be eliminated if as a member of a product Arg type

  trait Tracer[-I <: IUB] extends TracerLike {

    type In >: I <: IUB

    def map[O2](fn: Arg1[Out] => O2): AndThen[In, Out, O2] = {

      /**
        * [[TracingView]] turning upside-down
        */
      val _fn: FnImpl[Arg1[Out], O2] = _fromVanilla(fn)

      val result = _fn.^.apply[In, Out](this)

      result
    }

    def andThen[O2](fn: Arg1[Out] => O2): AndThen[In, Out, O2] = map(fn) // merely an alias

//    def flatMap
  }

  type TracerCompat[-I <: IUB, +O] = Tracer[_] { type In >: I; type Out <: O }

  trait Var[I <: IUB] extends Tracer[I] {

    type In = I
    type Out = I

    def apply(arg: In): Out = arg
  }
  protected def Var[I <: IUB]: Var[I] = new Var[I] {}

  trait Thunk extends Tracer[_Unit] {
    type In = _Unit
  }

  type ThunkCompat[+O] = Thunk { type Out <: O }

  // TODO: how to declare pure function?
  sealed trait Fn[-I <: IUB] extends Tracer[I] {

    case class Tracing() {

      val outer: Fn[I] { type In = Fn.this.In; type Out = Fn.this.Out } = Fn.this

      def apply[
          IPrev <: IUB,
          OPrev
      ](prev: TracerCompat[IPrev, OPrev])(
          implicit
          ev: Arg1[OPrev] <:< outer.In
      ): AndThen[IPrev, OPrev, outer.Out] = {

        val _this: FnCompat[Arg1[OPrev], outer.Out] = outer.widen[Arg1[OPrev], outer.Out]

        AndThen(prev, _this)
      }
    }

    def ^ : Tracing = Tracing()

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

    class Cached[I <: IUB, R](
        val composedFrom1: FnCompat[I, R]
    ) extends FnImpl[I, R]
        with Explainable.Composite1 {

      lazy val underlyingCache: CacheView[I, R] = Same.ByEquality.Lookup[I, R]()

      final def apply(key: I): R = {
        underlyingCache.getOrElseUpdateOnce(key)(composedFrom1(key))
      }

      final def getExisting(arg: I): Option[R] = {
        underlyingCache
          .get(arg)
      }
    }

    class AsFunction1[I <: IUB, R](val self: FnCompat[I, R]) extends (I => R) with Serializable {

      final override def apply(v1: I): R = self.apply(v1)

      final override def toString: String = self.toString // preserve reference transparency
    }

    implicit def _fnAsFunction1[I <: IUB, R](fn: FnCompat[I, R]): I => R = {
      new AsFunction1[I, R](fn)
    }

  }

  type FnCompat[-I <: IUB, +R] = Fn[_] { type In >: I; type Out <: R }

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

    def identity[I <: IUB]: FnImpl[I, I] = Fn(v => v)

  }

  // tracing/composition API

  /*
  preferred syntax:

  given Fn exp, plus, root

  val composite = Composite.create {
    // can use unapply

    case (x: Free[Double], y: Free[Double], n: Free[Int]) =>
      val xn = exp.^(x >< n)
      val yn = exp.^(x >< n)
      val xyn = plus.^(xn >< yn)
      val rootN = for (_x <- xyn;_n <- n) yield {_x ^ _n} TODO: how to define flatMap? With currying or tuple?
      rootN
  }

  all `.^` can be skipped implicitly
   */

  case class AndThen[
      I <: IUB,
      O1,
      O2
  ](
      on: TracerCompat[I, O1],
      fn: FnCompat[Arg1[O1], O2]
  ) extends FnImpl[I, O2]
      with Explainable.Composite
      with Explainable.DecodedName {

    def apply(arg: I): O2 = {
      val o1 = on.apply(arg)
      val _arg1: Arg1[O1] = arg1(o1)
      fn.apply(_arg1)
    }

    override def composedFrom: Seq[Explainable] = Seq(on, fn)
  }

  def trace[I <: IUB, F <: Fn[I]](
      fn: Var[I] => F
  ): F = {

    val free = Var[I]
    val result = fn(free)

    result
  }

  // shortcuts

  implicit def _fromVanilla[I <: IUB, R](
      vanilla: I => R
  ): FnImpl[I, R] = {
    implicit val definedAt: CallStackRef = definedHere

    vanilla match {
      case asFunction1: Fn.AsFunction1[I, R] =>
        asFunction1.self.asInstanceOf[FnImpl[I, R]]
      case _ =>
        Fn[I, R](vanilla)
    }
  }
}
