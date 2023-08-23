package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, GraphK}

trait Topology extends Law with Lawful {
  self: Singleton =>

  type Law_/\ >: this.type <: Law

  final def self: this.type = this

  type Graph[V] = GraphK.Aux[Law_/\, V]

  implicit def summon: this.type = this
}

object Topology {

  abstract class Of[L <: Law] extends Topology {
    self: L with Singleton =>

    type Law_/\ = L
  }

  trait AnyGraphT extends Law.Impl[Arrow]
  object AnyGraphT extends Of[AnyGraphT] with AnyGraphT {

    type _Arrow = Arrow

    trait OutboundT extends AnyGraphT with Law.Impl[Arrow.`~>`.^]
    object OutboundT extends Of[OutboundT] with OutboundT {

      type _Arrow = Arrow.`~>`.^
    }
  }

  trait PosetT extends AnyGraphT
  object PosetT extends Of[PosetT] with PosetT {

    type _Arrow = Arrow
  }

  trait SemilatticeT extends PosetT
  object SemilatticeT extends Of[SemilatticeT] with SemilatticeT {

    type _Arrow = Arrow

    trait UpperT extends SemilatticeT with AnyGraphT.OutboundT
    object UpperT extends Of[UpperT] with UpperT {

      type _Arrow = Arrow.`~>`.^

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  trait TreeT extends SemilatticeT.UpperT
  object TreeT extends Of[TreeT] with TreeT {

    type _Arrow = Arrow.`~>`.^
  }

  private def compileTimeCheck[V](): Unit = {

    implicitly[PosetT.Node[Int] <:< AnyGraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< AnyGraphT.Node[V]]
  }
}
