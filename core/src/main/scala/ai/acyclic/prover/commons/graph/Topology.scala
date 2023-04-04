package ai.acyclic.prover.commons.graph

trait Topology[A <: Arrow] {

  type _OpsBound[N] = Induction[N, A, Ops[N]]

  type Ops[N] <: _OpsBound[N]

  type _Graph[N] = GraphK[Ops[N]]
}

object Topology {

//  trait OpsBound[+A <: Arrow, N, +SELF <: OpsBound[A, N, SELF]] extends Induction.ArrowImpl[N, A, SELF] {
//
//    final type Node = N
//  }

  object GraphT extends Topology[Arrow] {

    trait Ops[N] extends _OpsBound[N] {

      final lazy val directEdges = induction.collect {
        case (v, _) if v.arrowType.isInstanceOf[Arrow.Edge] => v
      }

      def resolve(): Unit = {
        induction;
        nodeText
      }
    }

    object OutboundT extends Topology[Arrow.`~>`.^] {

      trait Ops[N] extends GraphT.Ops[N] with _OpsBound[N] {

        final lazy val children: Seq[Node] = {
          induction.map(v => v._2)
        }

        lazy val isLeaf: Boolean = children.isEmpty
      }
    }
  }

  object PosetT extends Topology[Arrow] {

    trait Ops[N] extends GraphT.Ops[N] with _OpsBound[N] {}
  }

  object SemilatticeT extends Topology[Arrow] {

    trait Ops[N] extends PosetT.Ops[N] with _OpsBound[N] {}

    object UpperT extends Topology[Arrow.`~>`.^] {

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

  object TreeT extends Topology[Arrow.`~>`.^] {

    trait Ops[N] extends SemilatticeT.UpperT.Ops[N] with _OpsBound[N] {}
  }

}
