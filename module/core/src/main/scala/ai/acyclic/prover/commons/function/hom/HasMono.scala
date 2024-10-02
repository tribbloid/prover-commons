package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.collection.CacheView
import ai.acyclic.prover.commons.same.Same
import ai.acyclic.prover.commons.util.SrcExplainable

object HasMono {}

trait HasMono extends HasPoly {

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
  trait Mono[
      -T_/\
  ] extends Poly {

    type In[_ <: T_/\]
    type Out[T <: T_/\]

    def apply[T <: T_/\](arg: In[T]): Out[T]

    implicit final def only[T <: T_/\]: In[T] =>> Out[T] = at[In[T]] { v =>
      apply(v)
    }
  }

  object Mono {

    class Cached[T_/\, SS <: Mono[T_/\]](
        val backbone: SS
    ) extends Mono[T_/\]
        with SrcExplainable.Composite1 {

      override type In[T <: T_/\] = backbone.In[T]
      override type Out[T <: T_/\] = backbone.Out[T]

      lazy val lookup: CacheView[Any, Any] = Same.Native.Lookup[Any, Any]()

      override def apply[T <: T_/\](arg: In[T]): Out[T] = {

        lookup
          .getOrElseUpdateOnce(arg)(
            backbone.apply(arg)
          )
          .asInstanceOf[Out[T]]
      }

      final def getExisting[T <: T_/\](arg: In[T]): Option[Out[T]] = {
        lookup
          .get(arg)
          .map { v =>
            v.asInstanceOf[Out[T]]
          }
      }
    }
  }

  implicit class MonoOps[
      T_/\,
      SS <: Mono[T_/\]
  ](val self: SS)
      extends Serializable {

    def cachedBy(
        _lookup: CacheView[Any, Any] = Same.Native.Lookup()
    ): Mono.Cached[T_/\, SS] = {

      type Result = Mono.Cached[T_/\, SS]

      val result: Result =
        new Mono.Cached[T_/\, SS](self) {

          override lazy val lookup: CacheView[Any, Any] = _lookup
        }
      result
    }
  }

  type MonoCompat[T_/\, -I[_ <: T_/\], +O[_ <: T_/\]] = Mono[T_/\] {
    type In[T <: T_/\] >: I[T]
    type Out[T <: T_/\] <: O[T]
  }

  trait MonoImpl[T_/\, I[_ <: T_/\], O[_ <: T_/\]] extends Mono[T_/\] {
    type In[T <: T_/\] = I[T]
    type Out[T <: T_/\] = O[T]
  }

  trait Dependent[
      T_/\
  ] extends Mono[T_/\] {

    type In[+T <: T_/\] = T
  }

  object Dependent {}

  /**
    * function with dependent type
    *
    * equivalent to `def(v: A): v.R` or `def[T <: A](v: T): T#R` in terms of capability
    * @tparam I
    *   type(s) of input arg(s)
    * @tparam R
    *   type constructor of output
    */
  type DependentCompat[T_/\, +O[_ <: T_/\]] = Dependent[T_/\] {
    type Out[T <: T_/\] <: O[T]
  }

  trait DependentImpl[T_/\, O[_ <: T_/\]] extends Dependent[T_/\] {
    type Out[T <: T_/\] = O[T]
  }

  implicit class fnIsMono[I, O](val backbone: FnCompat[I, O]) extends Mono[Any] with SrcExplainable.Composite1 {

    override type In[+_] = I
    override type Out[+_] = O

    override def apply[_](arg: I): O = backbone.apply(arg)
  }
}
