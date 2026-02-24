package ai.acyclic.prover.commons.jit.hom

import ai.acyclic.prover.commons.collection.CacheMagnet
import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.Hom
import ai.acyclic.prover.commons.jit.bound.Bound
import ai.acyclic.prover.commons.multiverse.CanEqual
import ai.acyclic.prover.commons.util.PredefKinds
import ai.acyclic.prover.commons.jit.eval.Args
import Args.{><:, T0}

object HasTypeLambda {}

/**
  * see https://ncatlab.org/nlab/show/unnatural+transformation
  */
trait HasTypeLambda extends HasPoly {
  self: Hom.type =>

  // keep it final to use Scala 3 type project to refer to inner classes without initialising it
  final case class BoundView[D <: Bound](bound: D) {

    /**
      * the most general form of poly1 in DOT calculus takes a bound and generate a function it should be cast into
      * TypeLambda in Scala 3. this is impossible in Scala 2 due to buggy bound inference
      */
    trait BoundLambda extends PolyLike {
      // TODO: need an implicit conversion from TypeLambda
      //  Scala 2 cannot figure out the correct bound for sub, cannot be a superclass

      def refine[B <: bound.Less](
          implicit
          bound: B
      ): Fn[?, ?]
    }

    object BoundLambda {

      type Gt[-B <: Bound] = BoundLambda { type Bound >: B }
    }

    /**
      * weaker than [[BoundLambda]], only works on a concrete type (instead of a bound). Major compiler spec upgrade
      * required, see
      *
      * https://stackoverflow.com/questions/79221926/in-scala-3-whats-the-meaning-of-unreducible-application-of-higher-kinded-type
      * https://github.com/scala/scala3/issues/22056
      */
    trait TypeLambda extends PolyLike {

      type In[T >: bound.Min <: bound.Max]
      type Out[T >: bound.Min <: bound.Max]

      def refine[T >: bound.Min <: bound.Max]: Fn[In[T] ><: T0, Out[T]]

      final def apply[T >: bound.Min <: bound.Max](arg: In[T] ><: T0): Out[T] = refine[T].apply(arg)

      def cached(byLookup: => CacheMagnet[Any, Any]): CachedLazy = {

        CachedLazy()(() => byLookup)
      }

      def cached(): CachedLazy = {
        CachedLazy()()
      }

      final case class CachedLazy()(
          getLookup: () => CacheMagnet[Any, Any] = () => CanEqual.Native.Lookup[Any, Any]()
      ) extends UnnaturalTransformation.Impl[In, Out] {

        def backbone: TypeLambda.this.type = TypeLambda.this

        lazy val lookup: CacheMagnet[Any, Any] = getLookup()

        override def refine[T >: bound.Min <: bound.Max]: Fn[In[T] ><: T0, Out[T]] = {

          val result = Fn.at[In[T]] { i =>
            lookup
              .getOrElseUpdateOnce(i) {

                // safe by construction: Const.Provided <: ConstantFn across path-dependent Hom
                backbone.apply[T](
                  Args.><:(Const.Provided(i).asInstanceOf[ConstantFn[In[T]]], Args.eye).asInstanceOf[In[T] ><: T0]
                )

              }
              .asInstanceOf[Out[T]] // safe by construction: cache stores Out[T] values, type erased by CacheMagnet
          }

          result
        }

        type _Out[T >: bound.Min <: bound.Max] = TypeLambda.this.Out[T]

        type _OutOpt[T >: bound.Min <: bound.Max] = Option[_Out[T]]

        object CachedOnly extends UnnaturalTransformation.Impl[In, _OutOpt] {

          override def refine[T >: bound.Min <: bound.Max]: Fn[In[T] ><: T0, Option[_Out[T]]] = {

            val result = Fn.at[In[T]] { i =>
              lookup
                .get(i)
                .map { v =>
                  v.asInstanceOf[_Out[T]] // safe by construction: cache stores _Out[T] values
                }
            }

            result
          }
        }
      }
    }

    type UnnaturalTransformation[
        -I[T >: bound.Min <: bound.Max],
        +O[T >: bound.Min <: bound.Max]
    ] = UnnaturalTransformation.Compat[I, O]

    case object UnnaturalTransformation {

      type Compat[
          -I[T >: bound.Min <: bound.Max],
          +O[T >: bound.Min <: bound.Max]
      ] = TypeLambda {

        type In[T >: bound.Min <: bound.Max] >: I[T]
        type Out[T >: bound.Min <: bound.Max] <: O[T]
      }

      abstract class Impl[
          I[T >: bound.Min <: bound.Max],
          O[T >: bound.Min <: bound.Max]
      ](
          implicit
          override val _definedAt: SrcDefinition
      ) extends TypeLambda {

        override type In[T >: bound.Min <: bound.Max] = I[T]
        override type Out[T >: bound.Min <: bound.Max] = O[T]
      }

      implicit class Is[I <: Args, O](backbone: Fn[I, O])
          extends Impl[PredefKinds.Drop1[_, I], PredefKinds.Drop1[_, O]]()(backbone._definedAt)
          with TypeLambda {

        override type In[T >: bound.Min <: bound.Max] = I
        override type Out[T >: bound.Min <: bound.Max] = O

        // safe by construction: backbone type I may already be _ ><: T0, compiler can't prove after erasure
        override def refine[T >: bound.Min <: bound.Max]: Fn[I ><: T0, O] = backbone.asInstanceOf[Fn[I ><: T0, O]]
      }
      //    implicit def _fnIsPoly1[I, O](fn: Circuit[I, O]): Is[I, O] = Is(fn)

    }

//    type Dependent[+O[_ >: bound.Min <: bound.Max]] = UnnaturalTransformation[PredefKinds.Invar, O]
//    // TODO: remove, superseded by DepFn
//    case object Dependent {
//
//      trait Impl[O[_ >: bound.Min <: bound.Max]] extends UnnaturalTransformation.Impl[PredefKinds.Invar, O]
//    }
  }

  object BoundView {

    val top: BoundView[Bound.Top] = BoundView[Bound.Top](Bound.Top)
  }

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
    * to apply the poly1 to a value of know type, simply refine it to a pinpoint bound [[Bound.PointAt[T]]
    *
    * this is the most larconic definition of a poly1 I can think of, but there may be more automated ways to figure out
    * refinement rule(s)
    */
}
