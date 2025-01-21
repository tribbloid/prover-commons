package ai.acyclic.prover.commons.collection

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
trait CacheMagnet[K, V] {
  import CacheMagnet.*
  // TODO: this should be a mixin to Map to minimise overhead

  def asMap: MapRepr[K, V]

  def asSet(
      implicit
      ev: Unit <:< V
  ): SetRepr[K]

  final def getOrElseUpdateOnce(key: K)(value: => V): V = {

    asMap.getOrElse(
      key, {
        this.synchronized {
          asMap.getOrElseUpdate(key, value)
        }
      }
    )
  }

  // TODO: impl to updatedWithFnSynced
}

object CacheMagnet {

  type MapRepr[K, V] = mutable.Map[K, V]

  type SetRepr[K] = mutable.Set[K]

  implicit def asMap[K, V](v: CacheMagnet[K, V]): MapRepr[K, V] = v.asMap
}
