package ai.acyclic.prover.commons.collection

import ai.acyclic.prover.commons.function.Bijection

import scala.collection.mutable

object ValueEncodedMap {

  //  class Immutable() TODO: impl this

  class Mutable[K, V, VV](
      codec: Bijection[V, VV],
      underlying: mutable.Map[K, VV] = mutable.Map.empty[K, VV]
  ) extends mutable.Map[K, V] {

    override def get(key: K): Option[V] =
      underlying.get(key).map(codec.invert)

    override def iterator: Iterator[(K, V)] = underlying.iterator.map(t => (t._1, codec.invert(t._2)))

    override def addOne(kv: (K, V)): this.type = {
      underlying.put(kv._1, codec(kv._2))
      this
    }

    override def subtractOne(key: K): this.type = {
      underlying.remove(key)
      this
    }
  }
}
