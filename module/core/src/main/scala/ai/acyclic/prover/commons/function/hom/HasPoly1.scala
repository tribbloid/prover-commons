package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.collection.LookupMagnet
import ai.acyclic.prover.commons.function.bound.TypeBound
import ai.acyclic.prover.commons.multiverse.CanEqual
import ai.acyclic.prover.commons.util.{K, SrcDefinition}

object HasPoly1 {}

trait HasPoly1 extends HasPoly {

  // keep it final to use Scala 3 type project to refer to inner classes without initialising it
  final case class BoundView[D <: TypeBound](bound: D) {

    type Poly1[
        -I[T >: bound.Min <: bound.Max],
        +O[T >: bound.Min <: bound.Max]
    ] = Poly1.Compat[I, O]

    case object Poly1 {

      /**
        * the most general form of poly1 in DOT calculus takes a bound and generate a function it should be cast into
        * TypeLambda in Scala 3. this is impossible in Scala 2 due to buggy bound inference
        */
      trait BoundLambda extends PolyLike {
        // TODO: need an implicit conversion from TyepLambda
        //  Scala 2 cannot figure out the correct bound for sub, cannot be a superclass

        type Bound <: bound.Less

        def refine[Sub <: Bound](sub: Sub): Fn[?, ?]
      }

      object BoundLambda {

        type Gt[-B <: TypeBound] = BoundLambda { type Bound >: B }
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

        def apply[T >: bound.Min <: bound.Max](arg: In[T]): Out[T]

        def cached(byLookup: => LookupMagnet[Any, Any]): CachedLazy = {

          CachedLazy()(() => byLookup)
        }

        def cached(): CachedLazy = {
          CachedLazy()()
        }

        final case class CachedLazy()(
            getLookup: () => LookupMagnet[Any, Any] = () => CanEqual.Native.Lookup[Any, Any]()
        ) extends Impl[In, Out]
            with ByRefine {

          def backbone: TypeLambda.this.type = TypeLambda.this

          @transient lazy val lookup: LookupMagnet[Any, Any] = getLookup()

          override def refine[T >: bound.Min <: bound.Max]: Fn[In[T], Out[T]] = {

            val result = Fn.at[In[T]] { i =>
              lookup
                .getOrElseUpdateOnce(i) {

                  backbone.apply[T](i)

                }
                .asInstanceOf[Out[T]]
            }

            result
          }

          type _Out[T >: bound.Min <: bound.Max] = TypeLambda.this.Out[T]

          type _OutOpt[T >: bound.Min <: bound.Max] = Option[_Out[T]]

          object CachedOnly extends Impl[In, _OutOpt] with ByRefine {

            override def refine[T >: bound.Min <: bound.Max]: Fn[In[T], Option[_Out[T]]] = {

              val result = Fn.at[In[T]] { i =>
                lookup
                  .get(i)
                  .map { v =>
                    v.asInstanceOf[_Out[T]]
                  }
              }

              result
            }
          }
        }
      }

      trait ByRefine extends TypeLambda {

        def refine[T >: bound.Min <: bound.Max]: Fn[In[T], Out[T]]

        final def apply[T >: bound.Min <: bound.Max](arg: In[T]): Out[T] = refine[T].apply(arg)
      }

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

      implicit class Is[I, O](backbone: Fn[I, O])
          extends Impl[K.Drop1[_, I], K.Drop1[_, O]]()(backbone._definedAt)
          with ByRefine {

        override type In[T >: bound.Min <: bound.Max] = I
        override type Out[T >: bound.Min <: bound.Max] = O

        override def refine[T >: bound.Min <: bound.Max]: Fn[I, O] = backbone
      }
      //    implicit def _fnIsPoly1[I, O](fn: Circuit[I, O]): Is[I, O] = Is(fn)

    }

    type Dependent[+O[_ >: bound.Min <: bound.Max]] = Poly1[K.Invar, O]
    // TODO: remove, superseded by DepFn
    case object Dependent {

      trait Impl[O[_ >: bound.Min <: bound.Max]] extends Poly1.Impl[K.Invar, O]
    }
  }

  object BoundView {

    val top: BoundView[TypeBound.Top] = BoundView[TypeBound.Top](TypeBound.Top)
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
    * to apply the poly1 to a value of know type, simply refine it to a pinpoint bound [[TypeBound.PointAt[T]]
    *
    * this is the most larconic definition of a poly1 I can think of, but there may be more automated ways to figure out
    * refinement rule(s)
    */
}
