package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Arrow

// used for structure-preserving rewrite of a graph into another graph of a different type
trait Transcriber[N, G <: Graph[N]] extends Builder[N, G] {

  lazy val graph: G = build(Nil)

  trait RewriteNodeOps {

    def setInductions(arrows: graph.Many[Arrow.Of[N]]): N

    final def setInductionsSafely(arrows: graph.Many[Arrow.Of[N]]): N = {

      val neo = setInductions(arrows)
      val actualInductions = graph.nodeOps(neo).induction

      require(arrows == actualInductions)

      neo
    }
  }
  def rewriteNode: N => RewriteNodeOps

  trait RewriteArrowOps {

    def setTarget(target: N): Arrow.Of[N]
  }
  def rewriteArrow: Arrow => RewriteArrowOps
}
