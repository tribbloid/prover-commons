package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.Same
import ai.acyclic.prover.commons.function.FnLike.Transparent1


object HasMorphism {}

trait HasMorphism extends HasPolyLike {

  // dedicated to polymorphic functions
  // Morphism takes type argument, Poly takes an implicit type class
  // relying heavily on kind-projector plugin: https://github.com/typelevel/kind-projector
  // please refrain from using shortened syntax as it is very different from that of Scala3
  // TODO: Unfortunately, from this point, shapeless Poly & DepFn are almost useless
  //  just like Scala2 Function is almost useless
  //  DepFn is not really a function with dependent type
  //  Poly is unbounded, doesn't have impl for Morphism, has poor compatibility with Scala3,
  //  also, each case doesn't have an output dependent type (like DepFn)

  /**
    * a.k.a. parametric polymorphism, e.g. natural transformation
    *
    * serve as the basis of functions with dependent type
    *
    * @tparam T_/\
    *   parameter's upper bound
    * @tparam I
    *   type constructor(s) of input arg(s)
    */
  trait Morphism[
      -T_/\
  ] extends FnLike {

    type In[_ <: T_/\] <: IUB
    type Out[T <: T_/\]

    def apply[T <: T_/\](arg: In[T]): Out[T]
  }

  object Morphism {

    trait Cached[T_/\, SS <: Morphism[T_/\]] extends Morphism[T_/\] with FnLike.Transparent1 {

      val reference: SS

      override type In[T <: T_/\] = reference.In[T]
      override type Out[T <: T_/\] = reference.Out[T]

      lazy val sameness: Same.By = Same.ByEquality
      lazy val correspondence = sameness.Correspondence[Any, Any]()

      override def apply[T <: T_/\](arg: In[T]): Out[T] = {

        correspondence
          .getOrElseUpdate(
            arg,
            reference.apply(arg)
          )
          .asInstanceOf[Out[T]]
      }
    }
  }

  type MorphismCompat[T_/\, -I[_ <: T_/\] <: IUB, +O[_ <: T_/\]] = Morphism[T_/\] {
    type In[T <: T_/\] >: I[T]
    type Out[T <: T_/\] <: O[T]
  }

  trait Dependent[
      T_/\ <: IUB
  ] extends Morphism[T_/\] {

    type In[+T <: T_/\] = T
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
  type DependentCompat[T_/\ <: IUB, +O[_ <: T_/\]] = Dependent[T_/\] {
    type Out[T <: T_/\] <: O[T]
  }

  implicit class fnIsMorphism[I <: IUB, O](val reference: FnCompat[I, O]) extends Morphism[Any] with Transparent1 {

    override type In[+_] = I
    override type Out[+_] = O

    override def apply[_](arg: I): O = reference.apply(arg)
  }
}
