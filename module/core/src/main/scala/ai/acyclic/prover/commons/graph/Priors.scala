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

    def value: Any

    def nodeText: String = value.toString

    /**
      * Only affecting caching mechanism in resolving induction(s). Induction of the same node may be cached and reused
      * instead of being computed twice. If returns None, no computation will ever be cached
      *
      * CAUTION: this won't affect node representation in diagrams, need to override the following [[identityC]]
      *
      * in general, [[evalCacheKeyC]] equality should be a sufficient condition of [[identityC]] equality
      *
      * @return
      *   key with equality & hashcode
      */
    protected def evalCacheKeyC: Option[Any] = Some(this)
    final lazy val evalCacheKey = evalCacheKeyC

    protected def identityC: Option[Any] = evalCacheKeyC
    final lazy val identity = identityC
    // TODO: this should be fold in value
    //  if not, it may define a single node with 2 conflicting values

  }

  trait Graph {

    val engine: Engine
  }

}
