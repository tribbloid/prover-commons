package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, GraphK}

trait Topology extends Lawful {

  type _Arrow <: Arrow
  type _Axiom = Axiom_/\ { type _Arrow = Topology.this._Arrow }

  final def axiom: _Axiom = Axiom.apply[_Axiom]
  //      final def self: this.type = this
}

object Topology {

  abstract class Impl[X <: Axiom] extends Topology {

    override type Axiom_/\ = X
  }

  abstract class HasTopology[X <: Axiom] extends Lawful {
    self: Singleton =>

    type Graph[v] = GraphK.Aux[Axiom_/\, v]

    override type Axiom_/\ = X

    abstract class _Topology extends Impl[X] {}

    type _Top <: _Topology
    def _Top: () => _Top

    implicit lazy val top: _Top = _Top.apply()
  }

  trait AnyGraphT extends Axiom.Impl[Arrow]
  object AnyGraphT extends HasTopology[AnyGraphT] {

    case class _Top() extends _Topology {
      type _Arrow = Arrow
    }

    trait OutboundT extends AnyGraphT with Axiom.Impl[Arrow.`~>`.^]
    object OutboundT extends HasTopology[OutboundT] {

      case class _Top() extends _Topology {
        type _Arrow = Arrow.`~>`.^
      }
    }

  }

  trait PosetT extends AnyGraphT
  object PosetT extends HasTopology[PosetT] {

    case class _Top() extends _Topology {
      type _Arrow = Arrow
    }
  }

  trait SemilatticeT extends PosetT
  object SemilatticeT extends HasTopology[SemilatticeT] {

    case class _Top() extends _Topology {
      type _Arrow = Arrow
    }

    trait UpperT extends SemilatticeT with AnyGraphT.OutboundT
    object UpperT extends HasTopology[UpperT] {

      case class _Top() extends _Topology {
        type _Arrow = Arrow.`~>`.^
      }

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  trait TreeT extends SemilatticeT.UpperT
  object TreeT extends HasTopology[TreeT] {

    case class _Top() extends _Topology {
      type _Arrow = Arrow.`~>`.^
    }
  }

  private def sanity[V](): Unit = {

    implicitly[Topology.Impl[TreeT]]

    implicitly[PosetT.Node[Int] <:< AnyGraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< AnyGraphT.Node[V]]

  }
}
