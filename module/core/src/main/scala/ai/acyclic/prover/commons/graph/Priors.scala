package ai.acyclic.prover.commons.graph

object Priors {

  trait HasBatch {

    trait IBatch[+V] {

      def isEmpty: Boolean

      def collect: Seq[V]

      def distinct: Batch[V]

      def flatMap[U](f: V => IterableOnce[U]): Batch[U]

      def map[U](f: V => U): Batch[U] = flatMap(v => Seq(f(v)))

      def union[V2 >: V](that: Batch[V2]): Batch[V2]
    }

    type Batch[+V] <: IBatch[V]
    def parallelize[T](seq: Seq[T]): Batch[T]
  }

  trait Node {

    lazy val identity: Option[Any] = evalCacheKey

    /**
      * Only affecting caching mechanism in resolving induction(s). Induction of the same node may be cached and reused
      * instead of being computed twice. If returns None, the cache will be ignored
      *
      * CAUTION: it is generally stronger than [[identity]], namely, [[evalCacheKey]] equality should be a sufficient
      * condition of [[identity]] equality, but not vice versa. E.g.1 identical node can have 2 different
      * [[evalCacheKey]], each referring to forward and backward links.
      *
      * @return
      *   key with equality & hashcode
      */
    lazy val evalCacheKey: Option[Any] = Some(this)

    def value: Any

    def nodeText: String = value.toString
  }

  trait Graph {

    val engine: Engine
  }
}
