package ai.acyclic.prover.commons.graph

trait NodeKind[+C <: Topology.Constraint, +A <: Arrow] {

  import NodeKind._

  type Value
  def value: Value // bound type of values of this node and all its descendants, NOT the type of this value!

  protected def getNodeText: String = value.toString
  final lazy val nodeText: String = getNodeText

  protected def getInduction: Seq[(A, NodeKind.Lt[C, A, Value])]
  lazy val induction = getInduction

  final lazy val discoverNodes: Seq[NodeKind.Lt[C, A, Value]] = induction.map(_._2)

  final lazy val inductionToValues: Seq[(A, Value)] = induction.map(v => v._1 -> v._2.value)

  final lazy val discoverValues: Seq[Value] = inductionToValues.map(_._2)

  def map[V2](fn: Value => V2): NodeKind.Aux[C, A, V2] = Mapped(this: NodeKind.Lt[C, A, Value], fn)
}

object NodeKind {

  type Aux[+C <: Topology.Constraint, +A <: Arrow, V] = NodeKind[C, A] { type Value = V }
  trait AuxT[+C <: Topology.Constraint, +A <: Arrow, V] extends NodeKind[C, A] { type Value = V }

  type Lt[+C <: Topology.Constraint, +A <: Arrow, +V] = Aux[C, A, _ <: V]

  trait Untyped[+C <: Topology.Constraint, +A <: Arrow] extends NodeKind[C, A] {
    // actually self typed, but that doesn't convey any extra information

    type Value >: this.type
    final lazy val value: this.type = this
  }

  case class Mapped[C <: Topology.Constraint, A <: Arrow, V, V2](
      original: NodeKind.Lt[C, A, V],
      fn: V => V2
  ) extends AuxT[C, A, V2] {

    override def value: V2 = fn(original.value)

    override protected def getNodeText: String = original.nodeText

    override protected def getInduction: Seq[(A, NodeKind.Aux[C, A, V2])] = {
      original.induction.map {
        case (a, n) =>
          a -> Mapped(n, fn)
      }
    }
  }
}
