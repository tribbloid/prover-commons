package ai.acyclic.prover.commons.compat

trait HasTupleX {

  type TupleX = shapeless.HList

  type *:[X, Y <: TupleX] = shapeless.::[X, Y]

  object TupleX { // TODO: merge into HList

    trait OpsMixin { // hollow inside, but mixin will bring _ops into the implicit scope
      self: TupleX =>
    }

    implicit class _ops[T <: TupleX](self: T) {

      def *:[H](h: H): H *: T = (h :: self).asInstanceOf[H *: T]
    }

    type Nil = shapeless.HNil
    protected val Nil: shapeless.HNil & OpsMixin = shapeless.HNil.asInstanceOf[shapeless.HNil & OpsMixin]

    type _1 = Nil
    val _1 = Nil

    val Unit: Nil = Nil
    type Unit = Nil

    type T1[T] = T *: Nil

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
