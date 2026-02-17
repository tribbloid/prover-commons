package ai.acyclic.prover.commons.compat

import ai.acyclic.prover.commons.tuple.Products

trait HasTupleX {

  type TupleX = shapeless.HList

  type *:[X, Y <: TupleX] = TupleX.><:[X, Y]

  object TupleX extends Products.Monoidal { // TODO: merge into HList

    override type VBound = Any
    override type Element[T] = T

    trait OpsMixin { // hollow inside, but mixin will bring _ops into the implicit scope
      self: TupleX =>
    }

    implicit class _ops[T <: TupleX](self: T) {

      def *:[H](h: H): *:[H, T] & OpsMixin = (h :: self).asInstanceOf[*:[H, T] & OpsMixin]
    }

    override type Prod = shapeless.HList

    type Eye = shapeless.HNil
    protected val Eye: shapeless.HNil & OpsMixin = shapeless.HNil.asInstanceOf[shapeless.HNil & OpsMixin]

    type Unit = Nil
    val Unit: Nil = Eye

    override infix type ><:[H, Tail <: Prod] = shapeless.::[H, Tail]

    type Builder = shapeless.ProductArgs
    object of extends Builder {

      def applyProduct[L <: TupleX](list: L): L = list
    }

    type Builder_narrow = shapeless.SingletonProductArgs

    object ofNarrow extends Builder_narrow {

      def applyProduct[L <: TupleX](list: L): L = list

    }

    type Mapper = shapeless.Poly1

  }
}
