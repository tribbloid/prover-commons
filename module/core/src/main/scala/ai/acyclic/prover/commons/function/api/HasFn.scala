package ai.acyclic.prover.commons.function.api

import ai.acyclic.prover.commons.collection.CacheView
import ai.acyclic.prover.commons.debug.Debug
import ai.acyclic.prover.commons.debug.Debug.CallStackRef
import ai.acyclic.prover.commons.same.Same

import scala.language.implicitConversions

trait HasBuilder {

  /**
    * will be Tuple-like in the future
    */
  type IUB // TODO: should be "Domain"

  trait Builder {

    protected type =>>[i <: IUB, o]

    def define[I <: IUB, R](fn: I => R)(
        implicit
        _definedAt: CallStackRef = definedHere
    ): I =>> R

    final def apply[I <: IUB, R](fn: I => R)( // alias
        implicit
        _definedAt: CallStackRef = definedHere
    ): I =>> R = define(fn)

    case class RefinedBuilder[I <: IUB, O]() {

      def at[i <: IUB]: RefinedBuilder[i, O] = RefinedBuilder()

      def to[o]: RefinedBuilder[I, o] = RefinedBuilder()
      final def =>>[o]: RefinedBuilder[I, o] = to

      final def define[o <: O](fn: I => o)(
          implicit
          _definedAt: CallStackRef = definedHere
      ): I =>> o = Builder.this.define(fn)

      final def apply[o <: O](fn: I => o)( // alias
          implicit
          _definedAt: CallStackRef = definedHere
      ): I =>> o = define(fn)

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

//      def trace[o <: O](
//          fn: Fn.Identity[I] => FnImpl[I, o]
//      ): FnImpl[I, o] = {
//
//        val id = Fn.Identity[I]()
//        val result = fn(id)
//
//        result
//      }
    }

    // similar to `at` in shapeless Poly1
    def at[I <: IUB]: RefinedBuilder[I, Any] = RefinedBuilder[I, Any]()
  }

  protected[function] def definedHere: CallStackRef = CallStackRef
    .below(condition = { v =>
      v.isDefinedAtClasses(classOf[HasFn]) || v.isArgDefault
    })
    .below(1)
}

trait HasFn extends HasBuilder {

  type _Unit <: IUB
  val _unit: _Unit // terminal object in cat theory, can be eliminated if as a member of a product Arg type

  sealed trait Tracer extends TracerLike {

    def unbox: Repr

  }

  object Tracer {

    case class Element[R](unbox: R) extends TracerImpl[R] {}

    case class CanApply[I <: IUB, O](unbox: FnCompat[I, O]) extends TracerImpl[FnCompat[I, O]] {

      def apply(arg: TracerCompat[I]): Tracer.Applied[I, O] = {

        Tracer.Applied(
          unbox,
          arg
        )
      }
    }

    case class Applied[I <: IUB, R](
        fn: FnCompat[I, R],
        arg: TracerCompat[I]
    ) extends TracerImpl[R] {

      override def unbox: R = fn(arg.unbox)
    }

  }

  implicit def _unbox[I](v: TracerCompat[I]): I = v.unbox

  type TracerCompat[+R] = Tracer { type Repr <: R }

  trait TracerImpl[R] extends Tracer {

    final type Repr = R
  }

  // TODO: how to declare pure function?

  sealed trait Fn extends Explainable {
    // thin wrapper of a tracer

    type In <: IUB
    type Out

    def apply(arg: In): Out

    def ^ = Tracer.CanApply(
      Tracer.Element[this.type](Fn.this)
    )
  }

  type FnCompat[-I <: IUB, +R] = Fn { type In >: I; type Out <: R }

  trait FnImpl[I <: IUB, R] extends Fn { // most specific

    type In = I
    type Out = R
  }

  object Fn extends Builder {

    override type =>>[i <: IUB, o] = FnImpl[i, o]

    override def define[I <: IUB, O](vanilla: I => O)(
        implicit
        _definedAt: Debug.CallStackRef
    ): I =>> O = {

      vanilla match {
        case ops: FnOps[I, O] =>
          ops.self.asInstanceOf[FnImpl[I, O]]
        case _ =>
          new Blackbox[I, O](vanilla, _definedAt)
      }
    }

    case class Identity[I <: IUB]() extends FnImpl[I, I] {

      override def apply(arg: I): I = arg
    }

    class Blackbox[I <: IUB, R](
        val fn: I => R,
        override val _definedAt: CallStackRef = definedHere
    ) extends FnImpl[I, R] {

      override def apply(arg: I): R = fn(arg)
    }

    class Cached[I <: IUB, R](
        val backbone: FnCompat[I, R]
    ) extends FnImpl[I, R]
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

  implicit class FnOps[I <: IUB, O](val self: FnCompat[I, O]) extends Serializable {

    override def toString: String = self.toString // preserve reference transparency

    def cachedBy(
        cache: CacheView[I, O] = Same.ByEquality.Lookup[I, O]()
    ): Fn.Cached[I, O] = {
      new Fn.Cached[I, O](self) {

        override lazy val underlyingCache: CacheView[I, O] = cache
      }
    }
  }

  implicit def reprToFn[I <: IUB, R](
      vanilla: I => R
  ): FnImpl[I, R] = {
    implicit val definedAt: CallStackRef = definedHere

    Fn[I, R](vanilla)
  }

//  trait Pure[F <: Fn[_]] {} // type case, TODO: don't know how to do it at the moment
//
//  object Pure {
//
//    implicit def cachedIsPure[C <: Fn.Cached[_, _]]: Pure[C] = new Pure[C] {}
//  }

//  object FnImpl {}

//  trait Thunk extends Fn[_Unit] { type In = _Unit }

//  trait Thunk[V] extends Tracer {
//
//    override type Repr = V
//
//    override lazy val toString: String = "" + unbox
//  }
//
//  trait Value[V] extends Thunk[V] {
//
//    override val unbox: V
//  }

  // shortcuts

}
