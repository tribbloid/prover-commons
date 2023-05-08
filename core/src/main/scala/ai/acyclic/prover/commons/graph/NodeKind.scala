package ai.acyclic.prover.commons.graph

trait NodeKind[+L <: Law] extends Lawful.Construct[L] {

  import NodeKind._

  def value: Value // bound type of values of this node and all its descendants, NOT the type of this value!

  protected def nodeTextC: String = value.toString
  final lazy val nodeText: String = nodeTextC

  private[this] type Compat = NodeKind.Compat[L, Value]

  protected def inductionC: Seq[(_A, Compat)]
  lazy val induction = inductionC

  final lazy val discoverNodes: Seq[Compat] = induction.map(_._2)

  final lazy val inductionToValues: Seq[(_A, Value)] = induction.map(v => v._1 -> v._2.value)

  final lazy val discoverValues: Seq[Value] = inductionToValues.map(_._2)

  def map[V2](fn: Value => V2): NodeKind.Aux[L, V2] = Mapped(this: Compat, fn)

  def upcast[V2](
      implicit
      ev: Value <:< V2
  ) = map((v: Value) => v: V2)

  object asIterable extends Iterable[Value] {

    override def iterator: Iterator[Value] =
      Iterator(value) ++ discoverNodes.iterator.flatMap(n => n.asIterable.iterator)
  }
}

object NodeKind {

  type Aux[+L <: Law, V] = NodeKind[L] { type Value = V }
  trait AuxEx[+L <: Law, V] extends NodeKind[L] { type Value = V }

  // Acronym of "Less Than"
  type Compat[+L <: Law, +V] = Aux[L, _ <: V]

  trait Untyped[+L <: Law] extends NodeKind[L] {
    // actually self typed, but that doesn't convey any extra information

    type Value >: this.type
    final lazy val value: this.type = this
  }

  case class Mapped[L <: Law, V, V2](
      original: NodeKind.Compat[L, V],
      fn: V => V2
  ) extends AuxEx[L, V2] {

    override val law: original.law.type = original.law

    override def value: V2 = fn(original.value.asInstanceOf)

    override protected def nodeTextC: String = original.nodeText

    override protected def inductionC: Seq[(_A, NodeKind.Aux[L, V2])] = {
      original.induction.map {
        case (a, n) =>
          a -> Mapped(n, fn)
      }
    }

  }
}
