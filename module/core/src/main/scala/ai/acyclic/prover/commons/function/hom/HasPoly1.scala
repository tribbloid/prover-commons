package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.collection.LookupMagnet
import ai.acyclic.prover.commons.same.Same
import ai.acyclic.prover.commons.util.SrcDefinition

import scala.language.implicitConversions

object HasPoly1 {}

trait HasPoly1 extends HasPoly {

  // a special case of Poly that takes type argument, instead of several implicit type classes
  // relying heavily on kind-projector plugin: https://github.com/typelevel/kind-projector
  // (plugin supports multiple syntaxes, please refrain from using shortened syntax, difficult to move to Scala3)

  // TODO: Unfortunately, from this point, shapeless Poly & DepFn are almost useless
  //  just like Scala2 Function is almost useless
  //  DepFn is not really a function with dependent type
  //  Poly is unbounded & has poor compatibility with Scala3,
  //  also, each case doesn't have an output dependent type (like DepFn)

  /**
    * a.k.a. parametric polymorphism, e.g. natural transformation
    *
    * serve as the basis of functions with dependent type
    *
    * @tparam T_/\
    *   parameter's upper bound
    */
  case object Poly1Like {

    abstract class Decreasing[
        -T_/\
    ](
        implicit
        override val _definedAt: SrcDefinition
    ) extends PolyLike {

      type In[_ <: T_/\]
      type Out[_ <: T_/\]

      def apply[T <: T_/\](arg: In[T]): Out[T]

      implicit final def only[T <: T_/\]: In[T] |- Out[T] = at[In[T]] { v =>
        apply(v)
      }
    }

    sealed trait Increasing[
        +T_\/
    ] extends PolyLike {

      type In[_ >: T_\/]
      type Out[T >: T_\/]

      def apply[T >: T_\/](arg: In[T]): Out[T]

      implicit final def only[T >: T_\/]: In[T] Target Out[T] = at[In[T]] { v =>
        apply(v)
      }
    }
  }

  type Poly1Like[-T_/\] = Poly1Like.Decreasing[T_/\]

  type Poly1[T_/\, -I[_ <: T_/\], +O[_ <: T_/\]] = Poly1Like[T_/\] {
    type In[T <: T_/\] >: I[T]
    type Out[T <: T_/\] <: O[T]
  }

  case object Poly1 {

    trait Impl[T_/\, I[_ <: T_/\], O[_ <: T_/\]] extends Poly1Like[T_/\] {
      type In[T <: T_/\] = I[T]
      type Out[T <: T_/\] = O[T]
    }

    case class Is[I, O](backbone: Circuit[I, O]) extends Poly1Like[Any] {

      override type In[+_] = I
      override type Out[+_] = O

      override def apply[T](arg: I): O = backbone.apply(arg)
    }

    implicit def fnIsPoly1[I, O](v: Circuit[I, O]): Poly1.Is[I, O] = Poly1.Is(v)

    final case class Cached[T_/\, SS <: Poly1Like[T_/\]](
        backbone: SS,
        getLookup: () => LookupMagnet[Any, Any] = () => Same.Native.Lookup[Any, Any]()
    ) extends Poly1Like[T_/\] {

      override type In[T <: T_/\] = backbone.In[T]
      override type Out[T <: T_/\] = backbone.Out[T]

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
    }
  }

  implicit class Poly1Ops[
      T_/\,
      SS <: Poly1Like[T_/\]
  ](val self: SS)
      extends Serializable {

    def cached(
        byLookup: => LookupMagnet[Any, Any] = Same.Native.Lookup()
    ): Poly1.Cached[T_/\, SS] = {

      type Result = Poly1.Cached[T_/\, SS]

      val result: Result =
        Poly1.Cached[T_/\, SS](self, () => byLookup)
      result
    }
  }

  sealed trait DependentLike[
      T_/\
  ] extends Poly1Like[T_/\] {

    type In[T <: T_/\] = T
  }

  /**
    * function with dependent type
    *
    * equivalent to `def(v: A): v.R` or `def[T <: A](v: T): T#R` in terms of capability
    * @tparam I
    *   type(s) of input arg(s)
    * @tparam R
    *   type constructor of output
    */
  type Dependent[T_/\, +O[_ <: T_/\]] = DependentLike[T_/\] {
    type Out[T <: T_/\] <: O[T]
  }
  object Dependent {

    trait Impl[T_/\, O[_ <: T_/\]] extends DependentLike[T_/\] {
      type Out[T <: T_/\] = O[T]
    }

    type Invar[T] = T

    trait Identity extends Impl[Any, Invar]
  }
}
