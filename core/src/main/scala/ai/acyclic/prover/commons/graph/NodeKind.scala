package ai.acyclic.prover.commons.graph

trait NodeKind[+C <: Topology.Constraint, +A <: Arrow, +V] {
  // V is the bound type of values of this node and all its descendants
  //  NOT the type of this value!

  def value: V

  protected def getNodeText: String = value.toString
  final lazy val nodeText: String = getNodeText

  final type Peer = NodeKind[C, A, V]

  final type ArrowsTo[T] = Seq[(A, T)]

  protected def getInduction: Seq[(A, Peer)]

  lazy val induction = getInduction

  final lazy val discoverNodes: Seq[Peer] = induction.map(_._2)

  final lazy val inductionToValues: Seq[(A, V)] = induction.map(v => v._1 -> v._2.value)

  final lazy val discoverValues: Seq[V] = inductionToValues.map(_._2)

  def map[V2](fn: V => V2): NodeKind.Mapped[C, A, V, V2] = NodeKind.Mapped(this, fn)
}

object NodeKind {

  case class Mapped[+C <: Topology.Constraint, +A <: Arrow, V, V2](
      original: NodeKind[C, A, V],
      fn: V => V2
  ) extends NodeKind[C, A, V2] {

    override def value: V2 = fn(original.value)

    override protected def getNodeText: String = original.nodeText

    override protected def getInduction: Seq[(A, NodeKind[C, A, _ <: V2])] = {
      original.induction.map {
        case (a, n) =>
          a -> Mapped(n, fn)
      }
    }
  }
}
