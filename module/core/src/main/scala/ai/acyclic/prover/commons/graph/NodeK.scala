package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Axioms, Lawful}

trait NodeK[+L <: Axioms] extends Lawful.Struct[L] {

  import NodeK._

  def value: Value

  protected def getNodeText: String = value.toString
  final lazy val nodeText: String = getNodeText

  private[this] type Node_~ = NodeK.Compat[L, Value]

  protected def getInduction: Seq[(_Arrow, Node_~)]
  lazy val induction: Seq[(_Arrow, Node_~)] = getInduction

  final lazy val discoverNodes: Seq[Node_~] = induction.map(_._2)

  final lazy val inductionToValues: Seq[(_Arrow, Value)] = induction.map(v => v._1 -> v._2.value)

  final lazy val discoverValues: Seq[Value] = inductionToValues.map(_._2)

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

  def map[V2](fn: Value => V2): Mapped[L, Value, V2] = Mapped(this: Node_~, fn)

  def upcast[V2](
      implicit
      ev: Value <:< V2
  ): Mapped[L, Value, V2] = map((v: Value) => v: V2)

  object asIterable extends Iterable[Value] {

    override def iterator: Iterator[Value] =
      Iterator(value) ++ discoverNodes.iterator.flatMap(n => n.asIterable.iterator)
  }
}

object NodeK {

  type Aux[+L <: Axioms, V] = NodeK[L] { type Value = V }
  trait Impl[+L <: Axioms, V] extends NodeK[L] { type Value = V }

  type Compat[+L <: Axioms, +V] = Aux[L, _ <: V]

  trait Untyped[+L <: Axioms] extends NodeK[L] {
    // actually self typed, but that doesn't convey any extra information

    type Value >: this.type
    final lazy val value: this.type = this
  }

  case class Mapped[+L <: Axioms, V, V2](
      original: NodeK.Compat[L, V],
      fn: V => V2
  ) extends Impl[L, V2] {

    override val axioms: original.axioms.type = original.axioms

    override def value: V2 = fn(original.value.asInstanceOf)

    override protected def getNodeText: String = original.nodeText

    override protected def getInduction: Seq[(_Arrow, Mapped[L, V, V2])] = {
      original.induction.map {
        case (a, n) =>
          a -> Mapped(n, fn)
      }
    }

    override def identityKeyC: Option[Any] = original.identityKey

    override def evalCacheKeyC: Option[Any] = original.evalCacheKey
  }
}
