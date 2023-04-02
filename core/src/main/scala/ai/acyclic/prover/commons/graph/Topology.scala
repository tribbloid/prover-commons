package ai.acyclic.prover.commons.graph

trait Topology[A[+n] <: Arrow.Of[n]] {

  type _OpsBound[N] = Topology.OpsBound[A, N, Ops[N]]

  type Ops[N] <: _OpsBound[N]

  type _Graph[N] = GraphK[Ops[N]]
}

object Topology {

  trait OpsBound[+A[+n] <: Arrow.Of[n], N, +SELF <: OpsBound[A, N, SELF]]
      extends Induction
      with Induction.Mixin[A, SELF] {

    final type Node = N
  }

  object GraphT extends Topology[Arrow.Of] {

    trait Ops[N] extends _OpsBound[N] {

      final lazy val directEdges = induction.collect {
        case v if v.arrowType.isInstanceOf[Arrow.Edge] => v
      }

      def resolve(): Unit = {
        induction;
        nodeText
      }
    }

    object OutboundT extends Topology[Arrow.`~>`.Of] {

      trait Ops[N] extends GraphT.Ops[N] with _OpsBound[N] {

        final lazy val children: Seq[Node] = {
          induction.map(v => v.target)
        }

        lazy val isLeaf: Boolean = children.isEmpty
      }
    }
  }

  object PosetT extends Topology[Arrow.Of] {

    trait Ops[N] extends GraphT.Ops[N] with _OpsBound[N] {}
  }

  object SemilatticeT extends Topology[Arrow.Of] {

    trait Ops[N] extends PosetT.Ops[N] with _OpsBound[N] {}

    object UpperT extends Topology[Arrow.`~>`.Of] {

      trait Ops[N] extends _OpsBound[N] with SemilatticeT.Ops[N] with GraphT.OutboundT.Ops[N] {

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

  object TreeT extends Topology[Arrow.`~>`.Of] {

    trait Ops[N] extends SemilatticeT.UpperT.Ops[N] with _OpsBound[N] {}
  }

}
