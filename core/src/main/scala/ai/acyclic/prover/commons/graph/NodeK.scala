package ai.acyclic.prover.commons.graph

trait NodeK[+L <: Law] extends Lawful.Construct[L] {

  import NodeK._

  def value: Value // bound type of values of this node and all its descendants, NOT the type of this value!

  protected def nodeTextC: String = value.toString
  final lazy val nodeText: String = nodeTextC

  private[this] type Compat = NodeK.Compat[L, Value]

  protected def inductionC: Seq[(_A, Compat)]
  lazy val induction = inductionC

  final lazy val discoverNodes: Seq[Compat] = induction.map(_._2)

  final lazy val inductionToValues: Seq[(_A, Value)] = induction.map(v => v._1 -> v._2.value)

  final lazy val discoverValues: Seq[Value] = inductionToValues.map(_._2)

  /**
    * Only affecting caching mechanism in resolving induction(s). Induction of the same node may be cached and reused
    * instead of being computed twice. If returns None, no computation will ever be cached
    *
    * CAUTION: this won't affect node representation in diagrams, need to override the following [[identityKey]]
    *
    * in general, [[evalCacheKey]] equality should be a sufficient condition of [[identityKey]] equality
    *
    * @return
    *   key with equality & hashcode
    */
  lazy val evalCacheKey: Option[Any] = Some(this)

  /**
    * Due to the inductive nature of this library it is possible to have connectivity information of one node to be
    * split in multiple Node instances, each providing only a subset. The connectivity of a node thus can only revealed
    * by aggregating several Node instances with the same identityKey
    *
    * If returns None, this node will be considered different from any other node
    *
    * this primarily affects visualisation, e.g. in Hasse & Linked hierarchy diagrams
    *
    * in general, [[identityKey]] equality should be a necessary condition of [[evalCacheKey]] equality
    *
    * @return
    *   key with equality & hashcode
    */
  lazy val identityKey: Option[Any] = evalCacheKey

  def map[V2](fn: Value => V2): Mapped[L, Value, V2] = Mapped(this: Compat, fn)

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

  type Aux[+L <: Law, V] = NodeK[L] { type Value = V }
  trait AuxEx[+L <: Law, V] extends NodeK[L] { type Value = V }

  // Acronym of "Less Than"
  type Compat[+L <: Law, +V] = Aux[L, _ <: V]

  trait Untyped[+L <: Law] extends NodeK[L] {
    // actually self typed, but that doesn't convey any extra information

    type Value >: this.type
    final lazy val value: this.type = this
  }

  case class Mapped[+L <: Law, V, V2](
      original: NodeK.Compat[L, V],
      fn: V => V2
  ) extends AuxEx[L, V2] {

    override val law: original.law.type = original.law

    override def value: V2 = fn(original.value.asInstanceOf)

    override protected def nodeTextC: String = original.nodeText

    override protected def inductionC = {
      original.induction.map {
        case (a, n) =>
          a -> Mapped(n, fn)
      }
    }

    override lazy val identityKey: Option[Any] = original.identityKey

    override lazy val evalCacheKey: Option[Any] = original.evalCacheKey

  }
}
