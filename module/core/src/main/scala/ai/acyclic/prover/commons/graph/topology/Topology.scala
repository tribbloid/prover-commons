package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, GraphK}

trait Topology extends Lawful {
  self: Singleton =>

  type Conj_/\ <: Conj

  final def self: this.type = this

  type Graph[V] = GraphK.Aux[Conj_/\, V]

  implicit def summon: this.type = this
}

object Topology {

  abstract class Impl[L <: Conj] extends Topology {
    self: Singleton =>

    type Conj_/\ = L
  }

  trait AnyGraphT extends Conj.Impl[Arrow]
  object AnyGraphT extends Impl[AnyGraphT] {

//    type _Arrow = Arrow

    trait OutboundT extends AnyGraphT with Conj.Impl[Arrow.`~>`.^]
    object OutboundT extends Impl[OutboundT] {

//      type _Arrow = Arrow.`~>`.^
    }
  }

  trait PosetT extends AnyGraphT
  object PosetT extends Impl[PosetT] {

//    type _Arrow = Arrow
  }

  trait SemilatticeT extends PosetT
  object SemilatticeT extends Impl[SemilatticeT] {

//    type _Arrow = Arrow

    trait UpperT extends SemilatticeT with AnyGraphT.OutboundT
    object UpperT extends Impl[UpperT] {

//      type _Arrow = Arrow.`~>`.^

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  trait TreeT extends SemilatticeT.UpperT
  object TreeT extends Impl[TreeT] {

//    type _Arrow = Arrow.`~>`.^
  }

  private def compileTimeCheck[V](): Unit = {

    implicitly[PosetT.Node[Int] <:< AnyGraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< AnyGraphT.Node[V]]
  }
}
