package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.local.Rewriter.WithNewInduction

/**
  * at this moment, only capable of rewriting `canDiscover`
  *
  * should be able to rewrite induction arrows in the future
  * @tparam N
  *   node type of a graph
  */
trait Rewriter[N] extends (N => WithNewInduction[N]) {

  case class VerifiedOn(graph: Graph[N]) extends Rewriter[N] {

    case class Applied(node: N) extends WithNewInduction[N] {

      def apply(newNs: Seq[N]): N = {
        // Only rewrite if necessary
        val originalNs = graph.ops(node).discoverNodes
        if (originalNs == newNs) {
          // no need to rewrite, just return node as-is
          return node
        }

        val result = Rewriter.this.apply(node).apply(newNs)

        val actualDiscover = graph.ops(result).discoverNodes
        require(
          actualDiscover == newNs,
          s"""Incompatible rewriter?
             |Rewrite result should be [${newNs.mkString(", ")}]
             |but it is actually [${actualDiscover.mkString(", ")}]""".stripMargin
        )

        result
      }
    }

    final override def apply(node: N): Applied = Applied(node)
  }
}

object Rewriter {

  type WithNewInduction[N] = Seq[N] => N

  case class DoNotRewrite[N]() extends Rewriter[N] {

    override def apply(v1: N): WithNewInduction[N] = { _ =>
      v1
    }
  }

}
