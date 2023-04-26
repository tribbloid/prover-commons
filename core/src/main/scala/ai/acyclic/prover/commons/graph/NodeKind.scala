package ai.acyclic.prover.commons.graph

trait NodeKind[+L <: Topology.Law, +A <: Arrow] {

  import NodeKind._

  type Value
  def value: Value // bound type of values of this node and all its descendants, NOT the type of this value!

  protected def nodeTextC: String = value.toString
  final lazy val nodeText: String = nodeTextC

  protected def inductionC: Seq[(A, NodeKind.Lt[L, A, Value])]
  lazy val induction = inductionC

  final lazy val discoverNodes: Seq[NodeKind.Lt[L, A, Value]] = induction.map(_._2)

  final lazy val inductionToValues: Seq[(A, Value)] = induction.map(v => v._1 -> v._2.value)

  final lazy val discoverValues: Seq[Value] = inductionToValues.map(_._2)

  def map[V2](fn: Value => V2): NodeKind.Aux[L, A, V2] = Mapped(this: NodeKind.Lt[L, A, Value], fn)

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

  type Top = NodeKind[_, _]

  type Aux[+L <: Topology.Law, +A <: Arrow, V] = NodeKind[L, A] { type Value = V }
  trait AuxEx[+L <: Topology.Law, +A <: Arrow, V] extends NodeKind[L, A] { type Value = V }

  // Acronym of "Less Than"
  type Lt[+C <: Topology.Law, +A <: Arrow, +V] = Aux[C, A, _ <: V]

  trait Untyped[+C <: Topology.Law, +A <: Arrow] extends NodeKind[C, A] {
    // actually self typed, but that doesn't convey any extra information

    type Value >: this.type
    final lazy val value: this.type = this
  }

  case class Mapped[C <: Topology.Law, A <: Arrow, V, V2](
      original: NodeKind.Lt[C, A, V],
      fn: V => V2
  ) extends AuxEx[C, A, V2] {

    override def value: V2 = fn(original.value)

    override protected def nodeTextC: String = original.nodeText

    override protected def inductionC: Seq[(A, NodeKind.Aux[C, A, V2])] = {
      original.induction.map {
        case (a, n) =>
          a -> Mapped(n, fn)
      }
    }
  }
}
