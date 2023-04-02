package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.Induction.Mixin

trait Topology[A[+n] <: Arrow.Of[n]] {

  type _Induction[N] = Topology.Actual[A, N, Impl[N]]

  type Impl[N] <: _Induction[N]

  type _Graph[N] = GraphK[Impl[N]]
}

object Topology {

  trait Actual[+A[+n] <: Arrow.Of[n], N, +SELF <: Actual[A, N, SELF]] extends Induction with Mixin[A, SELF] {

    final type Node = N
  }

  object GraphT extends Topology[Arrow.Of] {

    trait Impl[N] extends _Induction[N] {

      final lazy val directEdges = induction.collect {
        case v if v.arrowType.isInstanceOf[Arrow.Edge] => v
      }

      def resolve(): Unit = {
        induction;
        nodeText
      }
    }

    object OutboundT extends Topology[Arrow.`~>`.Of] {

      trait Impl[N] extends GraphT.Impl[N] with _Induction[N] {

        final lazy val children: Seq[Node] = {
          induction.map(v => v.target)
        }

        lazy val isLeaf: Boolean = children.isEmpty
      }
    }
  }

  object PosetT extends Topology[Arrow.Of] {

    trait Impl[N] extends GraphT.Impl[N] with _Induction[N] {}
  }

  object SemilatticeT extends Topology[Arrow.Of] {

    trait Impl[N] extends PosetT.Impl[N] with _Induction[N] {}

    object UpperT extends Topology[Arrow.`~>`.Of] {

      trait Impl[N] extends _Induction[N] with SemilatticeT.Impl[N] with GraphT.OutboundT.Impl[N] {

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

    trait Impl[N] extends SemilatticeT.UpperT.Impl[N] with _Induction[N] {}
  }

}
