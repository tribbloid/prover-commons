package ai.acyclic.prover.commons

import scala.collection.concurrent.TrieMap

case class Correspondence[K, V](fn: K => V) extends (K => V) {

  case class Wrapper(key: K) {

    lazy val value: V = fn(key)
  }

  val cache: TrieMap[Int, Wrapper] = TrieMap.empty[Int, Wrapper]

  final def getOrElseUpdate(key: K): V = {

    val inMemoryId = System.identityHashCode(key)
    val w = this.synchronized {

      cache.getOrElseUpdate(inMemoryId, Wrapper(key)) // should be fast
    }
    w.value
  }

  final def apply(key: K): V = getOrElseUpdate(key)
}
