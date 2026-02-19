package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.compat.NamedTupleX.:=
import shapeless.labelled.{field, FieldType}
import shapeless.tag.@@

package object compat extends HasTupleX {

  type XInt = Int & Singleton
  type XStr = String & Singleton

  type Lazy[+T] = shapeless.Lazy[T]

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
