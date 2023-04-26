package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.Same
import ai.acyclic.prover.commons.graph.plan.Plan
import ai.acyclic.prover.commons.graph.plan.local.GraphUnary

import scala.language.implicitConversions

trait Engine {
  self: Singleton =>

  import Topology._

  type Dataset[+T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  trait OnEngine {
    self: GraphKind[_, _] =>

    final override val engine: Engine.this.type = Engine.this
  }

  /**
    * Graph representation without any validation
    */
  case class Unlawful[L <: Law, A <: Arrow, V](
      entriesC: Dataset[NodeKind.Lt[L, A, V]],
      override val nodeSameness: Same.Definition = Same.ByEquality
      //      nodeText: V => String = (v: V) => v.toString,
      //      arrowText: Arrow => Option[String] = v => v.arrowText
  ) extends GraphKind.AuxT[L, A, V]
      with OnEngine {
    // TODO: implement Lawful variant which summons corresponding Topology.Law and validate the graph

    type Node = NodeKind.Lt[L, A, Value]
  }

  abstract class BuildTemplate[T <: Topology](val base: T) extends Topology {
    self: Singleton =>

    final type L = base.L
    final type A = base.A

    final type G[V] = GraphKind.Aux[L, A, V] with OnEngine

    case class Leaf[V](val cc: G[V]) extends Plan {
      override type OT = BuildTemplate.this.type
      override val outTopology = BuildTemplate.this

      override type OV = V

      override def compute: G[V] = cc
    }

    implicit def graphAsLeaf[V](g: G[V]): Leaf[V] = Leaf(g.asInstanceOf)

    implicit def graphAsUnary[V](
        self: G[V]
    ) = GraphUnary.make(graphAsLeaf(self))
  }

  abstract class FromRoots[T <: Topology](override val base: T) extends BuildTemplate(base) {
    self: Singleton =>

    def apply[CC <: L, AA <: A, V](
        nodes: NodeKind.Lt[CC, AA, V]*
    ): GraphKind.Aux[L, A, V] with OnEngine =
      Unlawful(parallelize(nodes))
  }

//  abstract class FromSingleRoot[T <: Topology](override val topology: T) extends BuildTemplate(topology) {
//    self: Singleton =>
//
//    def apply[CC <: L, AA <: A, V](
//        node: NodeKind.Lt[CC, AA, V]
//    ): OnEngine[CC, AA, V] =
//      Unlawful(parallelize[NodeKind.Lt[CC, AA, V]](Seq(node)))
//  }

  object Build extends FromRoots(AnyT)

  trait Syntax {

    object Graph extends FromRoots(GraphT) {

      object Outbound extends FromRoots(GraphT.OutboundT)
      type Outbound[V] = Outbound.G[V]
    }
    type Graph[V] = Graph.G[V]

    object Poset extends FromRoots(PosetT)
    type Poset[V] = Poset.G[V]

    object Semilattice extends FromRoots(SemilatticeT) {

      object Upper extends FromRoots(SemilatticeT.UpperT)
      type Upper[V] = Upper.G[V]
    }
    type Semilattice[V] = Semilattice.G[V]

    object Tree extends FromRoots(TreeT)
    type Tree[V] = Tree.G[V]
  }
  object Syntax extends Syntax
}

object Engine {}
