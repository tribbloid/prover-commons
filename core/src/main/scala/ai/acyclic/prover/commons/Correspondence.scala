package ai.acyclic.prover.commons

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

case class Correspondence[K, V](fn: K => V) extends (K => V) {

  case class Wrapper(key: K) {

    lazy val value: V = fn(key)
  }

  val lookup: TrieMap[Int, Wrapper] = TrieMap.empty[Int, Wrapper]
  val collection: mutable.Buffer[Wrapper] = mutable.Buffer.empty

  def values: Seq[V] = collection.map(_.value).toSeq

  final def getOrElseUpdate(key: K): V = {

    val inMemoryId = System.identityHashCode(key)
    val w = this.synchronized {

      lookup.getOrElseUpdate(
        inMemoryId, {
          val created = Wrapper(key)
          collection += created
          created
        }
      ) // should be fast
    }
    w.value
  }

  final def apply(key: K): V = getOrElseUpdate(key)
}
