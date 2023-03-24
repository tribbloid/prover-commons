package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.local.Rewriter.WithNewSuccessor

/**
  * at this moment, only capable of rewriting `canDiscover`
  *
  * should be able to rewrite induction arrows in the future
  * @tparam N
  *   node type of a graph
  */
trait Rewriter[N] extends (N => WithNewSuccessor[N]) {

  case class VerifiedOn(graph: Graph[N]) extends Rewriter[N] {

    case class Applied(node: N) extends WithNewSuccessor[N] {

      def apply(newDiscover: Seq[N]): N = {
        // Only rewrite if necessary
        val originalDiscover = graph.nodeOps(node).canDiscover
        if (originalDiscover == newDiscover) {
          // no need to rewrite, just return node as-is
          return node
        }

        val result = Rewriter.this.apply(node).apply(newDiscover)

        val actualDiscover = graph.nodeOps(result).canDiscover
        require(
          actualDiscover == newDiscover,
          s"""Incompatible rewriter?
             |Rewrite result should be [${newDiscover.mkString(", ")}]
             |but it is actually [${actualDiscover.mkString(", ")}]""".stripMargin
        )

        result
      }
    }

    final override def apply(node: N): Applied = Applied(node)
  }
}

object Rewriter {

  type WithNewSuccessor[N] = Seq[N] => N

  case class DoNotRewrite[N]() extends Rewriter[N] {

    override def apply(v1: N): WithNewSuccessor[N] = { _ =>
      v1
    }
  }

}
