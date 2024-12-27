package ai.acyclic.prover.commons.graph

object Priors {

  trait Node {

    def value: Any

    protected def getNodeText: String = value.toString
    final lazy val nodeText: String = getNodeText

    /**
      * Only affecting caching mechanism in resolving induction(s). Induction of the same node may be cached and reused
      * instead of being computed twice. If returns None, no computation will ever be cached
      *
      * CAUTION: this won't affect node representation in diagrams, need to override the following [[identityKeyC]]
      *
      * in general, [[evalCacheKeyC]] equality should be a sufficient condition of [[identityKeyC]] equality
      *
      * @return
      *   key with equality & hashcode
      */
    protected def evalCacheKeyC: Option[Any] = Some(this)
    final lazy val evalCacheKey = evalCacheKeyC

    /**
      * Due to the inductive nature of this library it is possible to have connectivity information of one node to be
      * split in multiple Node instances, each providing only a subset. The connectivity of a node thus can only
      * revealed by aggregating several Node instances with the same identityKey
      *
      * If returns None, this node will be considered different from any other node
      *
      * this primarily affects visualisation, e.g. in Flow & Linked hierarchy diagrams
      *
      * in general, [[identityKeyC]] equality should be a necessary condition of [[evalCacheKeyC]] equality
      *
      * @return
      *   key with equality & hashcode
      */
    protected def identityKeyC: Option[Any] = evalCacheKeyC
    final lazy val identityKey = identityKeyC
    // TODO: this should be fold in value
    //  if not, it may define a single node with 2 conflicting values

  }

  trait Graph {

    type _E <: Engine // TODO: should fold into engine using dependent type
    def engine: _E
  }

}
