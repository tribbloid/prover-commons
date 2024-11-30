package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons
import ai.acyclic.prover.commons.collection.LookupMagnet
import ai.acyclic.prover.commons.function
import ai.acyclic.prover.commons.function.TypeBound
import ai.acyclic.prover.commons.function.TypeBound.Any
import ai.acyclic.prover.commons.same.Same
import ai.acyclic.prover.commons.util.{Erased, SrcDefinition}

import scala.language.implicitConversions

object HasPoly1 {}

trait HasPoly1 extends HasPoly {

  /**
    * key observation:
    *
    * every polymorphic function (I[T] => O[T]) can degrade to a universal function with the widest bound:
    *
    * I[_ >: Nothing <: Any] => O[_ >: Nothing <: Any]
    *
    * (or in case the bound is explicitly defined to be narrower):
    *
    * I[_ >: sub.Min <: sub.Max] => O[_ >: sub.Min <: sub.Max]
    *
    * but unlike a common function, it has the capability to refine itself given a new bound, so if sub2 <: sub.Lt is
    * given:
    *
    * J (with the follwing definition) can be derived from I
    *
    * I[_ >: sub2.Min <: sub2.Max] => O[_ >: sub2.Min <: sub2.Max]
    *
    * to apply the poly1 to a value of know type, simply refine it to a pinpoint bound [[TypeBound.PointAt[T]]
    *
    * this is the most larconic definition of a poly1 I can think of, but there may be more automated ways to figure out
    * refinement rule(s)
    */

  type Poly1[-B <: TypeBound] = Poly1.TypeLambda { type BB = B }

  object Poly1 {

    /**
      * the most general form of poly1 in DOT calculus takes a bound and generate a function it should be cast into
      * TypeLambda in Scala 3. this is impossible in Scala 2 due to buggy bound inference
      */
    abstract class BoundLambda(
        implicit
        override val _definedAt: SrcDefinition
    ) extends PolyLike {

      val typeDomain: TypeBound
      type Bound <: typeDomain.Less

      def refine[Sub <: Bound](sub: Sub): Circuit[?, ?]
    }

    object BoundLambda {

      type Gt[-B <: TypeBound] = BoundLambda { type Bound >: B }

      implicit class View(self: BoundLambda {})
    }

    // TODO: Scala 2 cannot figure out the correct bound for sub, cannot extend Base directly

    /**
      * weaker than [[BoundLambda]], only works on a concrete type (instead of a bound). Major compiler spec upgrade
      * required, see
      *
      * https://stackoverflow.com/questions/79221926/in-scala-3-whats-the-meaning-of-unreducible-application-of-higher-kinded-type
      * https://github.com/scala/scala3/issues/22056
      */
    abstract class TypeLambda(
        implicit
        override val _definedAt: SrcDefinition
    ) extends BoundLambda {

      type Bound = typeDomain.Less & TypeBound.Point

      type Lambda[T >: typeDomain.Min <: typeDomain.Max] <: Circuit[?, ?]
//      def lambda[Sub <: Bound](sub: Sub): Lambda[sub.Point]

    }

    implicit class Is[I, O](backbone: Circuit[I, O]) extends TypeLambda()(backbone.definedAt) {

      override val typeDomain = TypeBound.Any

      override type Lambda[T >: typeDomain.Min <: typeDomain.Max] = Circuit[I, O]

      override def refine[Sub <: Bound](sub: Sub): Circuit[I, O] = backbone
    }
//    implicit def _fnIsPoly1[I, O](fn: Circuit[I, O]): Is[I, O] = Is(fn)

    final case class Cached[T_/\, TSelf <: TypeLambda](
        backbone: TSelf,
        getLookup: () => LookupMagnet[Any, Any] = () => Same.Native.Lookup[Any, Any]()
    ) extends TypeLambda {

      override val typeDomain: TypeBound = backbone.typeDomain

      override type Lambda = Circuit.Cached[]

//      override type In[T <: T_/\] = backbone.In[T]
//      override type Out[T <: T_/\] = backbone.Out[T]

      @transient lazy val lookup: LookupMagnet[Any, Any] = getLookup()

      override def apply[T <: T_/\](arg: In[T]): Out[T] = {

        lookup
          .getOrElseUpdateOnce(arg)(
            backbone.apply(arg)
          )
          .asInstanceOf[Out[T]]
      }

      def getExisting[T <: T_/\](arg: In[T]): Option[Out[T]] = {
        lookup
          .get(arg)
          .map { v =>
            v.asInstanceOf[Out[T]]
          }
      }

//      override type Lambda = this.type

//      override def refine[Sub <: typeDomain.Less with TypeBound.Point](sub: Sub): Circuit = ???
    }
  }

  object Dependent {

    abstract class Base(
        implicit
        override val _definedAt: SrcDefinition
    ) extends Poly1.TypeLambda {

      type Out[T >: typeDomain.Min <: typeDomain.Max]
      type Lambda[T >: typeDomain.Min <: typeDomain.Max] = Circuit[T, Out[T]]
    }
  }

  class BoundView[B <: TypeBound](val maxBound: B) {

    // most trait definitions has to be moved out to get implicits into scope

//    import maxBound.*

//    trait _Base extends Poly1.BoundLambda[maxBound.Less] {
//      final val typeDomain = BoundView.this.maxBound
//    }

//    abstract class OfKind[F[_ >: Min <: Max] <: Circuit[?, ?]](
//        implicit
//        override val _definedAt: SrcDefinition
//    ) extends _Base {
//
//      def refine[B <: bound.Lt](sub: B): F[? >: sub.Min <: sub.Max]
//
//      case class Only[B <: bound.Lt](sub: B) extends CircuitRelay {
//
//        override val lemma: Circuit[B, F[? >: sub.Min <: sub.Max]] = {
//          refine[B]
//        }
//      }
//      override def only[B <: bound.Lt](sub: B): Only[B] = Only(sub)
//    }
//
//    object OfKind {
//
//      case class Is[I, O](backbone: Circuit[I, O]) extends OfKind[Is[I, O]#F] {
//
//        type F[_] = Circuit[I, O]
//
//        override def refine[B <: bound.Lt](sub: B): Circuit[I, O] = backbone
//      }
//    }

//    object Dependent {
//
//      type _FromOutK[O[_ >: Min <: Max]] = Circuit[_, O[_]]
//    }
//
//    type Dependent[O[_ >: Min <: Max]] = OfKind[Dependent._FromOutK[O]]

  }

  object Unbounded extends BoundView {

    val maxBound: Top = TypeBound.Any
  }

  /**
    * a.k.a. parametric polymorphism, e.g. natural transformation
    *
    * serve as the basis of functions with dependent type
    */
  //  case object Poly1 {
  //
  //    import ai.acyclic.prover.commons.function.TypeBound.*
  //
  //    trait Common extends Poly1 {
  //
  //      type F[T >: bound.Min <: bound.Max] <: Circuit[?, ?]
  //
  //      override val projection: F[? >: bound.Min <: bound.Max]
  //
  //      case class Refined[Sub <: bound.Lt](sub: Sub) extends Common {
  //
  //        final override val bound: TypeBound = sub
  //        val outer = Common.this
  //
  //        override type F[T >: bound.Min <: bound.Max] = outer.F[T]
  //
  //        override protected def _definedAt: SrcDefinition = Common.this._definedAt
  //
  //        override val projection = outer.projection.asInstanceOf[F[? >: bound.Min <: bound.Max]]
  //      }
  //
  //      override def refine[Sub <: bound.Lt](sub: Sub): Refined[Sub] = Refined[Sub](sub)
  //    }
  //
  //    trait Sanity extends Common {
  //      // encoding T => Seq[T]
  //
  //      type F[T >: bound.Min <: bound.Max] = T |- Seq[T]
  //
  //      override val projection = at[_ >: ] { v =>
  //        Seq(v)
  //      }
  //    }
  //
  ////    trait Canonical extends Poly1Like {
  ////
  ////      type F[T] <: Circuit[? >: bound.Max, ? <: bound.Min]
  ////
  ////      def refine[B <: bound.Lt](sub: B): F[? >: sub.Min <: sub.Max]
  ////
  ////      case class Only[B <: bound.Lt](sub: B) extends Circuit.TProj {
  ////
  ////        override val projection: Circuit[B, F[? >: sub.Min <: sub.Max]] = {
  ////          refine[B]
  ////        }
  ////      }
  ////      override def only[B <: bound.Lt](sub: B): Only[B] = Only(sub)
  ////
  ////    }
  //  }

  //  type Mono[T_/\, -I[_ <: T_/\], +O[_ <: T_/\]] = MonoLike[T_/\] {
  //    type In[T <: T_/\] >: I[T]
  //    type Out[T <: T_/\] <: O[T]
  //  }
  //
  //  case object Mono {
  //
  //    trait Impl[T_/\, I[_ <: T_/\], O[_ <: T_/\]] extends MonoLike[T_/\] {
  //      type In[T <: T_/\] = I[T]
  //      type Out[T <: T_/\] = O[T]
  //    }
  //
  //    case class Is[I, O](backbone: Circuit[I, O]) extends MonoLike[Any] {
  //
  //      override type In[+_] = I
  //      override type Out[+_] = O
  //
  //      override def apply[T](arg: I): O = backbone.apply(arg)
  //    }
  //
  //    implicit def fnIsMono[I, O](v: Circuit[I, O]): Mono.Is[I, O] = Mono.Is(v)
  //
  //    final case class Cached[T_/\, SS <: MonoLike[T_/\]](
  //        backbone: SS,
  //        getLookup: () => LookupMagnet[Any, Any] = () => Same.Native.Lookup[Any, Any]()
  //    ) extends MonoLike[T_/\] {
  //
  //      override type In[T <: T_/\] = backbone.In[T]
  //      override type Out[T <: T_/\] = backbone.Out[T]
  //
  //      @transient lazy val lookup: LookupMagnet[Any, Any] = getLookup()
  //
  //      override def apply[T <: T_/\](arg: In[T]): Out[T] = {
  //
  //        lookup
  //          .getOrElseUpdateOnce(arg)(
  //            backbone.apply(arg)
  //          )
  //          .asInstanceOf[Out[T]]
  //      }
  //
  //      def getExisting[T <: T_/\](arg: In[T]): Option[Out[T]] = {
  //        lookup
  //          .get(arg)
  //          .map { v =>
  //            v.asInstanceOf[Out[T]]
  //          }
  //      }
  //    }
  //  }

  //  implicit class MonoOps[
  //      T_/\,
  //      SS <: MonoLike[T_/\]
  //  ](val self: SS)
  //      extends Serializable {
  //
  //    def cached(
  //        byLookup: => LookupMagnet[Any, Any] = Same.Native.Lookup()
  //    ): Mono.Cached[T_/\, SS] = {
  //
  //      type Result = Mono.Cached[T_/\, SS]
  //
  //      val result: Result =
  //        Mono.Cached[T_/\, SS](self, () => byLookup)
  //      result
  //    }
  //  }
  //
  //  type DependentLike[
  //      T_/\
  //  ] = MonoLike[T_/\] {
  //
  //    type In[T <: T_/\] = T
  //  }
  //
  //  /**
  //    * function with dependent type
  //    *
  //    * equivalent to `def(v: A): v.R` or `def[T <: A](v: T): T#R` in terms of capability
  //    * @tparam I
  //    *   type(s) of input arg(s)
  //    * @tparam R
  //    *   type constructor of output
  //    */
  //  type Dependent[T_/\, +O[_ <: T_/\]] = DependentLike[T_/\] {
  //    type Out[T <: T_/\] <: O[T]
  //  }
  //
  //  object Dependent {
  //
  //    type Invar[T] = T
  //
  //    trait Impl[T_/\, O[_ <: T_/\]] extends Mono.Impl[T_/\, Invar, O] {}
  //
  //    object Identity extends Impl[Any, Invar] {
  //      override def apply[T <: Any](arg: T): T = arg
  //    }
  //    type Identity = Identity.type
  //  }

}
