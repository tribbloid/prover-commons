package ai.acyclic.prover.commons.graph.processing

import ai.acyclic.prover.commons.graph.{Graph, GraphSystem}

case class GraphPlans[S <: GraphSystem](override val graphSys: S) extends Plans {

  import graphSys._

  trait TraversePlan extends Unary

  // NOT ForeachNode! Traversal may visit a node multiple times.
  case class Traverse(
      maxDepth: Int = Int.MaxValue,
      down: Node => Unit = { _: Node => {} },
      up: Node => Unit = { _: Node => {} },
      sortFn: Seq[Node] => Seq[Node] = identity
  ) {

    object DepthFirst extends TraversePlan {

      override def exeOn(node: Node): Unit = {

        down(node)

        node.discover.foreach { arrow =>
          val target = arrow.target
          Traverse.this.copy(maxDepth = maxDepth - 1).DepthFirst.exeOn(target)
        // TODO: may incur high overhead
        }

        up(node)
      }

    }
  }

}

object GraphPlans extends GraphPlans(Graph)
