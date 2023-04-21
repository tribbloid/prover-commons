package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.Same

trait Platform {
  self: Singleton =>

  import Topology._

  type Dataset[T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  trait ThisGraphKind[+C <: Constraint, +A <: Arrow, V] extends GraphKind[C, A, V] {

    final override val platform: Platform.this.type = Platform.this
  }

  case class GraphK[+C <: Constraint, +A <: Arrow, V](
      override val roots: Dataset[NodeKind[C, A, V]],
      override val sameness: Same.Definition = Same.ByEquality
      //      nodeText: V => String = (v: V) => v.toString,
      //      arrowText: Arrow => Option[String] = v => v.arrowText
  ) extends ThisGraphKind[C, A, V]

  abstract class BuildTemplate[T <: Topology](val latch: T) {

    final type G[V] = ThisGraphKind[latch.C, latch.A, V]

    def apply[C <: latch.C, A <: latch.A, V](
        nodes: NodeKind[C, A, V]*
    ): GraphK[C, A, V] =
      GraphK(parallelize(nodes))
  }

  object Build extends BuildTemplate(AnyT)

  trait Aliases {

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
  object Aliases extends Aliases
}

object Platform {}
