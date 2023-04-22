package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.Same

trait Engine {
  self: Singleton =>

  import Topology._

  type Dataset[+T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  trait GraphOnEngine[+C <: Constraint, +A <: Arrow, V] extends GraphKind[C, A, V] {

    final override val engine: Engine.this.type = Engine.this

  }

  case class GraphImpl[C <: Constraint, A <: Arrow, V](
      override val roots: Dataset[NodeKind.Lt[C, A, V]],
      override val sameness: Same.Definition = Same.ByEquality
      //      nodeText: V => String = (v: V) => v.toString,
      //      arrowText: Arrow => Option[String] = v => v.arrowText
  ) extends GraphOnEngine[C, A, V] {

    override type Peer = GraphImpl[C, A, V]
    override type Node = NodeKind.Aux[C, A, V]
  }

  abstract class BuildTemplate[T <: Topology](val topology: T) extends Topology {
    self: Singleton =>
    final type C = topology.C
    final type A = topology.A

    final type Node[V] = NodeKind.Lt[C, A, V]

    final type G[V] = GraphOnEngine[C, A, V]

    def apply[CC <: C, AA <: A, V](
        nodes: NodeKind.Lt[CC, AA, V]*
    ): GraphOnEngine[CC, AA, V] =
      GraphImpl(parallelize(nodes))
  }

  object Build extends BuildTemplate(AnyT)

  trait Syntax {

    object Graph extends BuildTemplate(GraphT) {

      object Outbound extends BuildTemplate(GraphT.OutboundT)
      type Outbound[V] = Outbound.G[V]
    }
    type Graph[V] = Graph.G[V]

    object Poset extends BuildTemplate(PosetT)
    type Poset[V] = Poset.G[V]

    object Semilattice extends BuildTemplate(SemilatticeT) {

      object Upper extends BuildTemplate(SemilatticeT.UpperT)
      type Upper[V] = Upper.G[V]
    }
    type Semilattice[V] = Semilattice.G[V]

    object Tree extends BuildTemplate(TreeT)
    type Tree[V] = Tree.G[V]
  }
  object Syntax extends Syntax
}

object Engine {}
