package ai.acyclic.prover.commons.graph

trait Topology extends Lawful {
  self: Singleton =>

  type ArrowBound <: Arrow

  type GraphLike[V] = GraphKind.Aux[_L, V]

  implicit def summon: this.type = this
}

object Topology {

  object GraphT extends Topology {

    type ArrowBound = Ar

    trait _L extends Law
    val law: _L = new _L {
      type _A = Arrow
    }

    object OutboundT extends Topology {

      trait _L extends GraphT._L {
        type _A <: Arrow.`~>`.^
      }
      val law: _L = new _L {
        type _A = Arrow.`~>`.^
      }
    }
  }

  object PosetT extends Topology {

    trait _L extends GraphT._L
    val law: _L = new _L {
      type _A = Arrow.`~>`.^
    }
  }

  object SemilatticeT extends Topology {

    trait _L extends PosetT._L
    val law: _L = new _L {
      type _A = Arrow.`~>`.^
    }

    object UpperT extends Topology {

      trait _L extends SemilatticeT._L with GraphT.OutboundT._L
      val law: _L = new _L {
        type _A = Arrow.`~>`.^
      }

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  object TreeT extends Topology {

    trait _L extends SemilatticeT.UpperT._L
    val law: _L = new _L {
      type _A = Arrow.`~>`.^
    }
  }

  private def compileTimeCheck[V](): Unit = {

    implicitly[PosetT.Node[Int] <:< GraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< GraphT.Node[V]]
  }
}
