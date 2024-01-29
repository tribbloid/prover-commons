package ai.acyclic.prover.commons.util

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * a magnet class that can be cast into scala [[scala.collection.concurrent.Map]] or [[scala.collection.mutable.Set]]
  * depending on consumption
  *
  * @tparam K
  *   key
  * @tparam V
  *   value
  */
trait CacheView[K, V] {
  import CacheView._

  def asMap: MapRepr[K, V]

  def asSet(default: V): SetRepr[K]

  def asSet()(
      implicit
      ev: Default[V]
  ): SetRepr[K] = asSet(ev.default)

  def getOrElseUpdateSynchronously(key: K)(value: => V): V = {

    asMap.getOrElse(
      key, {
        this.synchronized {
          asMap.getOrElseUpdate(key, value)
        }
      }
    )
  }
}

object CacheView {

  type MapRepr[K, V] = scala.collection.concurrent.Map[K, V]

  type SetRepr[K] = mutable.Set[K]

  implicit def asMap[K, V](v: CacheView[K, V]): MapRepr[K, V] = v.asMap
}
