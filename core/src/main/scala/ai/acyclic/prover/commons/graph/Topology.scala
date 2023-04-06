package ai.acyclic.prover.commons.graph

trait Topology[A <: Arrow] {

  type _OpsBound[V] = Induction[V, A, Ops[V]]

  type Ops[V] <: _OpsBound[V]

  type _Graph[V] = GraphK[Ops[V]]
}

object Topology {

  object GraphT extends Topology[Arrow] {

    trait Ops[V] extends _OpsBound[V] {

      final lazy val discoverEdges = discoverArrows.collect {
        case (v, _) if v.arrowType.isInstanceOf[Arrow.Edge] => v
      }

      def resolve(): Unit = {
        discoverArrows;
        nodeText
      }
    }

    object OutboundT extends Topology[Arrow.`~>`.^] {

      trait Ops[V] extends GraphT.Ops[V] with _OpsBound[V] {

        final lazy val children: Seq[V] = {
          discoverArrows.map(v => v._2)
        }

        lazy val isLeaf: Boolean = children.isEmpty
      }
    }
  }

  object PosetT extends Topology[Arrow] {

    trait Ops[V] extends GraphT.Ops[V] with _OpsBound[V] {}
  }

  object SemilatticeT extends Topology[Arrow] {

    trait Ops[V] extends PosetT.Ops[V] with _OpsBound[V] {}

    object UpperT extends Topology[Arrow.`~>`.^] {

      trait Ops[V] extends _OpsBound[V] with SemilatticeT.Ops[V] with GraphT.OutboundT.Ops[V] {

        //      def ops: Node => Impl

        //      lazy val allOffsprings: Seq[Node] = {
        //        val cc = children
        //        val others = children.flatMap { child =>
        //          ops(child).allOffsprings
        //        }
        //        cc ++ others
        //      }
      }
    }
  }

  object TreeT extends Topology[Arrow.`~>`.^] {

    trait Ops[V] extends SemilatticeT.UpperT.Ops[V] with _OpsBound[V] {}
  }

}
