package ai.acyclic.prover.commons.collection

import ai.acyclic.prover.commons.function.Bijection

import scala.collection.mutable

object KeyEncodedMap {

//  class Immutable() TODO: impl this

  class Mutable[K, KK, V](
      codec: Bijection[K, KK],
      underlying: mutable.Map[KK, V] = mutable.Map.empty[KK, V]
  ) extends mutable.Map[K, V] {

    final override def get(key: K): Option[V] =
      underlying.get(codec(key))

    final override def iterator: Iterator[(K, V)] = underlying.iterator.map(t => (codec.invert(t._1), t._2))

    final override def addOne(kv: (K, V)): this.type = {
      underlying.put(codec(kv._1), kv._2)
      this
    }

    final override def subtractOne(key: K): this.type = {
      underlying.remove(codec(key))
      this
    }
  }
}
