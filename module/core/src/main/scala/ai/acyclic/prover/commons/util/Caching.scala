package ai.acyclic.prover.commons.util

import com.github.benmanes.caffeine.cache.Caffeine

import scala.collection.mutable
import scala.language.implicitConversions

trait Caching {

  import scala.jdk.CollectionConverters._

  trait CacheTag

  def toBuilder(proto: Caffeine[Any, Any]): Caffeine[Any, Any]

  lazy val builder: Caffeine[Any, Any] = {
    val proto: Caffeine[Any, Any] = Caffeine.newBuilder()
    toBuilder(proto)
  }

  /**
    * A cache designed for multithreaded usage in cases where values (not keys) contained in this map will not
    * necessarily be cleanly removed. This map uses weak references for contained values (not keys) in order to ensure
    * that the existance of a reference to that object in this cache will not prevent the garbage collection of the
    * contained object.
    *
    * <p><b>Warning:</b> DO NOT use .weakKeys()!. Otherwise, the resulting map will use identity ({@code ==}) comparison
    * to determine equality of keys, which is a technical violation of the {@link Map} specification, and may not be
    * what you expect.
    *
    * @throws IllegalStateException
    *   if the key strength was already set
    * @see
    *   WeakReference
    */
  type ConcurrentCache[K, V] = mutable.Map[K, V] with CacheTag

  def ConcurrentCache[K, V](): ConcurrentCache[K, V] = {

    // TODO: switching to ben-mane's caffeine, the fastest JVM caching library
    //  and move to prover-commons
    val base = builder
      .build[K, V]()
      .asMap()

    val asScala: mutable.Map[K, V] = base.asScala

    asScala.asInstanceOf[ConcurrentCache[K, V]]
  }
}

object Caching {

  import scala.jdk.CollectionConverters._

  trait ConcurrentTag

  def javaConcurrentMap[K, V]() = new java.util.concurrent.ConcurrentHashMap[K, V]()

  type ConcurrentMap[K, V] = mutable.Map[K, V] with ConcurrentTag
  def ConcurrentMap[K, V](): ConcurrentMap[K, V] = {
    val result: mutable.Map[K, V] = javaConcurrentMap[K, V]().asScala
    result.asInstanceOf[ConcurrentMap[K, V]]
  }

  type ConcurrentSet[V] = mutable.Set[V] with ConcurrentTag
  def ConcurrentSet[V](): ConcurrentSet[V] = {

    val proto: mutable.Set[V] = javaConcurrentMap[V, Unit]().keySet(()).asScala
    proto.asInstanceOf[ConcurrentSet[V]]
  }

  object Strong extends Caching {
    override def toBuilder(proto: Caffeine[Any, Any]): Caffeine[Any, Any] = proto
  }

  object Weak extends Caching {

    override def toBuilder(proto: Caffeine[Any, Any]): Caffeine[Any, Any] = proto.weakValues()
  }

  object Soft extends Caching {

    override def toBuilder(proto: Caffeine[Any, Any]): Caffeine[Any, Any] = proto.softValues()
  }

  type ConcurrentCache[K, V] = Soft.ConcurrentCache[K, V]
  implicit def defaultImpl(v: this.type): Soft.type = Soft
}
