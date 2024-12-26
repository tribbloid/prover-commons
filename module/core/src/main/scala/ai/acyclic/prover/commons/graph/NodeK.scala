package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.Induction

trait NodeK[+X <: Induction] extends NodeOrGraph[X] {

  import NodeK.*

  type _Arrow = _Axiom#_Arrow

  def value: Value

  protected def getNodeText: String = value.toString
  final lazy val nodeText: String = getNodeText

  private[this] type NodeLt = NodeK.Lt[_Axiom, Value]

  type FBound = (_Arrow, NodeLt)

  def induction: Seq[FBound]

  final lazy val discoverNodes: Seq[NodeLt] = induction.map(_._2)

//  final lazy val link_values: Seq[(_Arrow, Value)] = induction.map(v => v._1 -> v._2.value)

//  final lazy val values: Seq[Value] = arrows_values.map(_._2)

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
    * split in multiple Node instances, each providing only a subset. The connectivity of a node thus can only revealed
    * by aggregating several Node instances with the same identityKey
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

  def map[V2](fn: Value => V2): Mapped[X, Value, V2] = Mapped(this, fn)

  def upcast[V2](
      implicit
      ev: Value <:< V2
  ): Mapped[X, Value, V2] = map((v: Value) => v: V2)

  object asIterable extends Iterable[Value] {

    override def iterator: Iterator[Value] =
      Iterator(value) ++ discoverNodes.iterator.flatMap(n => n.asIterable.iterator)
  }
}

object NodeK {

  type Aux[+L <: Induction, V] = NodeK[L] { type Value = V }
  trait Aux_[+L <: Induction, V] extends NodeK[L] { type Value = V }

  type Lt[+L <: Induction, +V] = NodeK[L] { type Value <: V }

  trait Untyped[+L <: Induction] extends NodeK[L] {
    // actually self typed, but that doesn't convey any extra information

    type Value >: this.type
    final lazy val value: this.type = this
  }

  case class Mapped[+X <: Induction, V, V2](
      original: NodeK.Lt[X, V],
      fn: V => V2
  ) extends Aux_[X, V2] {

    override type _Arrow = original._Arrow

    override def value: V2 = fn(original.value.asInstanceOf)

    override protected def getNodeText: String = original.nodeText

    override protected def induction: Seq[(_Arrow, Mapped[X, V, V2])] = {
      original.induction.map {
        case (a, n) =>
          a -> Mapped(n, fn)
      }
    }

    override def identityKeyC: Option[Any] = original.identityKey

    override def evalCacheKeyC: Option[Any] = original.evalCacheKey
  }
}
