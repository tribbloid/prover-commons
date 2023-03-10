package ai.acyclic.prover.commons

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

case class Correspondence[K, V]() {

  case class Thunk(fn: () => V) {

    lazy val value: V = fn()
  }

  val lookup: TrieMap[Int, Thunk] = TrieMap.empty[Int, Thunk]
  val collection: mutable.Buffer[Thunk] = mutable.Buffer.empty

  def values: Seq[V] = collection.map(_.value).toSeq

  final def getOrElseUpdate(key: K, getValue: () => V): V = this.synchronized {

    val inMemoryId = System.identityHashCode(key)
    val w = this.synchronized {

      lookup.getOrElseUpdate(
        inMemoryId, {
          val created = Thunk(getValue)
          collection += created
          created
        }
      ) // should be fast
    }
    w.value
  }

  case class Memoize(fn: K => V) extends (K => V) with HasOuter {

    override def outer = Correspondence.this

    final def apply(key: K): V = getOrElseUpdate(key, () => fn(key))
  }
}
