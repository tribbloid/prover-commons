package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.compat.NamedTupleX.:=
import shapeless.labelled.{field, FieldType}
import shapeless.tag.@@

package object compat {

  type XInt = Int & Singleton
  type XStr = String & Singleton

  type TupleX = shapeless.HList
  object TupleX {

    trait OpsMixin { // hollow inside, but mixin will bring _ops into the implicit scope
      self: TupleX =>
    }

    implicit class _ops[T <: TupleX](self: T) {

      def *:[H](h: H): H *: T = (h :: self).asInstanceOf[H *: T]
    }

    type T0 = shapeless.HNil
    val T0: T0 & OpsMixin = shapeless.HNil.asInstanceOf[T0 & OpsMixin]

    type *:[X, Y <: TupleX] = shapeless.::[X, Y]

    type T1[T] = T *: T0

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

  type Lazy[+T] = shapeless.Lazy[T]

  object NamedTupleX {

    type ->>[K, +V] = FieldType[K, V]

    type :=[K, +V] = (Symbol @@ K) ->> V

    type Builder = shapeless.RecordArgs

    object of extends Builder {

      def applyRecord[L <: TupleX](list: L): L = list
    }

    // TODO: impl Builder_narrow
  }

  class Key[K] {

    def ->>[V](value: V): FieldType[K, V] = field[K](value)

    def :=[V](value: V)(
        implicit
        ev: K <:< String
    ): K := V = value.asInstanceOf[K := V]

  }

  object Key {

    type Tag[K] = Symbol @@ K

    def apply[K] = new Key[K]
  }

}
