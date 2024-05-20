package ai.acyclic.prover.commons.function.api

import ai.acyclic.prover.commons.collection.CacheView
import ai.acyclic.prover.commons.debug.Debug
import ai.acyclic.prover.commons.debug.Debug.CallStackRef
import ai.acyclic.prover.commons.same.Same

import scala.language.implicitConversions

trait HasFn extends HasBuilder {

  type _Unit <: IUB
  val _unit: _Unit // terminal object in cat theory, can be eliminated if as a member of a product Arg type

  sealed trait Tracer extends TracerLike {

    def unbox: Repr

  }

  object Tracer {

    case class Blackbox[R](unbox: R) extends TracerImpl[R] {}

    case class Applied[I <: IUB, R](
        fn: FnCompat[I, R],
        arg: TracerCompat[I]
    ) extends TracerImpl[R] {

      override def unbox: R = fn(arg.unbox)
    }
  }

  implicit class TracerApply[I <: IUB, O](self: TracerCompat[FnCompat[I, O]]) {

    def apply(arg: TracerCompat[I]): Tracer.Applied[I, O] = {

      Tracer.Applied(
        self.unbox,
        arg
      )
    }

    def tracerApply(arg: TracerCompat[I]): Tracer.Applied[I, O] = apply(arg)
  }

  implicit def _unbox[I](v: TracerCompat[I]): I = v.unbox

  type TracerCompat[+R] = Tracer { type Repr <: R }

  trait TracerImpl[R] extends Tracer {

    final type Repr = R
  }

  sealed trait Fn extends Explainable {
    // thin wrapper of a tracer

    type In <: IUB
    type Out

    def apply(arg: In): Out

    def ^ : Tracer.Blackbox[Fn.this.type] =
      Tracer.Blackbox[Fn.this.type](Fn.this)
  }

  object Fn extends Builder {

    override type =>>[i <: IUB, o] = FnImpl[i, o]

    override def define[I <: IUB, O](vanilla: I => O)(
        implicit
        _definedAt: Debug.CallStackRef
    ): I =>> O = {

      vanilla match {
        case ops: FnReprLike[I, O] =>
          ops.asFn
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

    trait Pure[I <: IUB, R] extends FnImpl[I, R] {}

    object Pure {
      // user need to manually verify purity

      class ^[I <: IUB, R](self: FnCompat[I, R]) extends Pure[I, R] {
        override def apply(arg: I): R = self.apply(arg)
      }
    }

    class Cached[I <: IUB, R](
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

  type FnCompat[-I <: IUB, +R] = Fn { type In >: I; type Out <: R }

  trait FnImpl[I <: IUB, R] extends Fn { // most specific

    type In = I
    type Out = R
  }

  implicit class FnOps[I <: IUB, O](val self: FnCompat[I, O]) extends Serializable {

    override def toString: String = self.toString // preserve reference transparency

    def widen[i <: I, o >: O]: FnImpl[i, o] = this.asInstanceOf[FnImpl[i, o]]

    def cachedBy(
        cache: CacheView[I, O] = Same.ByEquality.Lookup[I, O]()
    ): Fn.Cached[I, O] = {
      new Fn.Cached[I, O](self) {

        override lazy val underlyingCache: CacheView[I, O] = cache
      }
    }
  }

  trait FnReprLike[I <: IUB, O] extends Serializable with (I => O) {

    def asFn: FnImpl[I, O]
  }

  implicit def vanillaToFn[I <: IUB, R](
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
