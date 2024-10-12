package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.collection.CacheView
import ai.acyclic.prover.commons.collection.CacheView.{MapRepr, SetRepr}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}

import java.util.concurrent.ConcurrentHashMap

trait Caching extends Serializable {

  trait CacheTag

  def build[K, V](): CacheView[K, V] with CacheTag
}

object Caching {

  import scala.jdk.CollectionConverters._

  object Maps extends Caching {

    private def javaConcurrentMap[K, V]() = new java.util.concurrent.ConcurrentHashMap[K, V]()

    case class Impl[K, V]() extends CacheView[K, V] with CacheTag {

      val underlying: ConcurrentHashMap[K, V] = {
        javaConcurrentMap[K, V]()
      }

      @transient lazy val asMap: MapRepr[K, V] = underlying.asScala

      override def asSet(
          implicit
          ev: Unit <:< V
      ): SetRepr[K] = underlying.keySet((): V).asScala
    }

    override def build[K, V](): Impl[K, V] = Impl()

  }

  trait CaffeineCaching extends Caching {

    def toBuilder(proto: Caffeine[Any, Any]): Caffeine[Any, Any]

    lazy val underlyingBuilder: Caffeine[Any, Any] = {
      val proto: Caffeine[Any, Any] = Caffeine.newBuilder()
      toBuilder(proto)
    }

    case class Impl[K, V]() extends CacheView[K, V] with CacheTag {

      val underlying: Cache[K, V] = underlyingBuilder.build[K, V]()

      @transient lazy val asMap: MapRepr[K, V] =
        underlying.asMap().asScala

      def asSet(
          implicit
          ev: Unit <:< V
      ): SetRepr[K] = {
        underlying.asMap().keySet().asScala
      }
    }

    override def build[K, V](): Impl[K, V] = Impl[K, V]()
  }

  object Strong extends CaffeineCaching {
    override def toBuilder(proto: Caffeine[Any, Any]): Caffeine[Any, Any] = proto

  }

  object Weak extends CaffeineCaching {

    override def toBuilder(proto: Caffeine[Any, Any]): Caffeine[Any, Any] = proto.weakValues()
  }

  object Soft extends CaffeineCaching {

    override def toBuilder(proto: Caffeine[Any, Any]): Caffeine[Any, Any] = proto.softValues()
  }

  type ConcurrentCache[K, V] = Soft.Impl[K, V]
  def ConcurrentCache[K, V](): ConcurrentCache[K, V] = Soft.build[K, V]()

  type ConcurrentMap[K, V] = Maps.Impl[K, V]
  def ConcurrentMap[K, V](): ConcurrentMap[K, V] = Maps.build[K, V]()

  type ConcurrentSet[K] = SetRepr[K]
  def ConcurrentSet[K](): SetRepr[K] = Maps.build[K, Unit]().asSet
}
