package ai.acyclic.prover.commons.graph

trait Topology extends Lawful {
  self: Singleton =>

  type ArrowUB <: Arrow

  override type _L <: Law { type _A <: ArrowUB }

  trait LawImpl extends Law {
    type _A = ArrowUB
  }

  type GraphLike[V] = GraphKind.Aux[_L, V]

  implicit def summon: this.type = this
}

object Topology {

  object GraphT extends Topology {

    type ArrowUB = Arrow
    trait _L extends Law

    object OutboundT extends Topology {

      type ArrowUB = Arrow.`~>`.^
      trait _L extends GraphT._L {
        type _A <: Arrow.`~>`.^
      }

    }
  }

  object PosetT extends Topology {

    type ArrowUB = Arrow
    trait _L extends GraphT._L
  }

  object SemilatticeT extends Topology {

    type ArrowUB = Arrow
    trait _L extends PosetT._L

    object UpperT extends Topology {

      type ArrowUB = Arrow.`~>`.^
      trait _L extends SemilatticeT._L with GraphT.OutboundT._L

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  object TreeT extends Topology {

    type ArrowUB = Arrow.`~>`.^
    trait _L extends SemilatticeT.UpperT._L

  }

  private def compileTimeCheck[V](): Unit = {

    implicitly[PosetT.Node[Int] <:< GraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< GraphT.Node[V]]
  }
}
