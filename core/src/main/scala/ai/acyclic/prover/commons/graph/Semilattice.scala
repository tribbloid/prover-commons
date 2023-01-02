package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.viz.Hierarchy

object Semilattice {

  object Upper extends GraphSystem {

    trait OpsOf[+PEER <: Node] extends Poset.OpsOf[PEER] with Graph.Outbound.OpsOf[PEER] {

      lazy val allOffsprings: Seq[PEER] = {
        val cc: Seq[PEER] = children
        val others: Seq[PEER] = children.flatMap { child =>
          child.allOffsprings
            .map(v => v.asInstanceOf[PEER]) // typer fumbling here
        }
        cc ++ others
      }

//      case class PeerOps()(
//          implicit
//          ev: this.type <:< PEER
//      ) {
//
//        object DepthFirst {
//
//          def foreachNode(
//              maxDepth: Int = Int.MaxValue,
//              down: PEER => Unit = { _ => {} },
//              up: PEER => Unit = { _ => {} }
//          ): Unit = {}
//        }
//
//      }

//      def depthFirst(
//          implicit
//          ev: this.type <:< PEER
//      ) = PeerOps().DepthFirst
    }

    trait Node extends Graph.Outbound.Node with Poset.Node with OpsOf[Node] {

      def showHierarchy(
          implicit
          format: Hierarchy = Hierarchy.default
      ): format.Viz[Node.this.type] = format.Viz(this)
    }

  }
}
