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

  type _Unit <: IUB
  val _unit: _Unit // terminal object in cat theory, can be eliminated if as a member of a product Arg type

  trait Tracer extends TracerLike {

    def unbox: Repr

    def map[O2](fn: Repr => O2): TracerImpl[O2] = {

      new TracerImpl[O2] {

        def unbox: O2 = fn.apply(Tracer.this.unbox)
      }
    }

    def foreach(fn: Repr => Unit): Unit = map(fn).unbox

    def flatMap[O2](fn: Repr => TracerCompat[O2]): TracerImpl[O2] = {
      ???
    }
  }

  implicit def doubleElimination[Inner <: Tracer](v: TracerCompat[Inner]): Inner = v.unbox

  type TracerCompat[+R] = Tracer { type Repr <: R }

  trait TracerImpl[R] extends Tracer {

    final type Repr = R
  }

  trait Value[R] extends TracerImpl[R] {

    override val unbox: R
  }

//  // TODO: how to declare pure function?
//  sealed trait Fn[-I <: IUB] extends Tracer {
//
//    type In >: I <: IUB
//    type Out
//
//    type Repr = In => Out
//    final override def unbox: Repr = FnOps(this)
//
//    /**
//      * the only Single Abstract Method interface
//      * @param arg
//      *   input (can be product type)
//      * @return
//      *   output (can be curried function)
//      */
//    def apply(arg: In): Out
//
////    case class Tracing() extends Value[Fn.this.type] {
////
////      override val unbox: Fn.this.type = Fn.this
////
////      val outer: Fn[I] { type In = Fn.this.In; type Out = Fn.this.Out } = Fn.this
////
////      def apply[
////          On
////      ](prev: TracerCompat[On])(
////          implicit
////          ev: Arg1[On] <:< outer.In
////      ): FnImpl[IPrev, outer.Out] = {
////
////        val _this: FnCompat[Arg1[On], outer.Out] = outer.widen[Arg1[On], outer.Out]
////
////        Fn.Compose(prev, _this).reduce
////      }
////    }
////
////    // lifting operator, calling it can convert any symbol into tracing mode ...
////    // that can compose into other programs in the next compilation stage
////    def ^ : Tracing = Tracing()
//
//    case class Then() {
//
//      def map[O2](next: Arg1[Out] => O2): FnImpl[In, O2] = {
//
//        /**
//          * [[TracingView]] turning upside-down
//          */
//        val nextFn: FnImpl[Arg1[Out], O2] = Fn(next)
//
//        val result: FnImpl[In, O2] =
//          Fn.Compose(Fn.this: FnCompat[In, Out], nextFn).reduce
//
//        result
//      }
//
//      def foreach(fn: Arg1[Out] => Unit): FnImpl[In, Unit] = map(fn)
//
//      def apply[O2](fn: Arg1[Out] => O2): FnImpl[In, O2] = map(fn) // merely an alias
//
//      def flatMap[O2](fn: Arg1[Out] => FnCompat[_Unit, O2]): FnImpl[In, O2] = {
//        ???
//      }
//    }
//
//    def andThen[O2](fn: Arg1[Out] => O2): FnImpl[In, O2] = Then().map(fn) // merely an alias
//  }

  trait Fn extends Explainable with Tracer {
    // thin wrapper of a tracer

    type In <: IUB
    type Out

    // always self-tracing
    type Repr = this.type
    override def unbox = this

    def apply(arg: In): Out
  }

  type FnCompat[-I <: IUB, +R] = Fn { type In >: I; type Out <: R }

  trait FnImpl[I <: IUB, R] extends Fn { // most specific

    type In = I
    type Out = R
  }

  object Fn {

    case class Identity[I <: IUB]() extends FnImpl[I, I] {

      override def apply(arg: I): I = arg

    }

    class Blackbox[I <: IUB, R](
        val fn: I => R,
        override val _definedAt: CallStackRef = definedHere
    ) extends FnImpl[I, R] {

      override def apply(arg: I): R = fn(arg)
    }

    def apply[I <: IUB, O](
        vanilla: I => O
    )(
        implicit
        definedAt: CallStackRef = definedHere
    ): FnImpl[I, O] = {

      vanilla match {
        case ops: FnOps[I, O] =>
          ops.self.asInstanceOf[FnImpl[I, O]]
        case _ =>
          new Blackbox[I, O](vanilla, definedAt)
      }
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

    trait CanBuild {

      protected type =>>[i <: IUB, o]

      def define[I <: IUB, R](fn: I => R)(
          implicit
          _definedAt: CallStackRef = definedHere
      ): I =>> R

      final def apply[I <: IUB, R](fn: I => R)( // alias
          implicit
          _definedAt: CallStackRef = definedHere
      ): I =>> R = define(fn)

      case class Builder[I <: IUB, O]() {

        def at[i <: IUB]: Builder[i, O] = Builder()

        def to[o]: Builder[I, o] = Builder()
        final def =>>[o]: Builder[I, o] = to

        final def define[o <: O](fn: I => o)(
            implicit
            _definedAt: CallStackRef = definedHere
        ): I =>> o = CanBuild.this.define(fn)

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

        def trace[o <: O](
            fn: Fn.Identity[I] => FnImpl[I, o]
        ): FnImpl[I, o] = {

          val id = Fn.Identity[I]()
          val result = fn(id)

          result
        }
      }

      // similar to `at` in shapeless Poly1
      def at[I <: IUB]: Builder[I, Any] = Builder[I, Any]()
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
