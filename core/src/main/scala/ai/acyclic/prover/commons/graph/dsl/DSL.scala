package ai.acyclic.prover.commons.graph.dsl

import ai.acyclic.prover.commons.graph.local.{Graph, Transcriber}

trait DSL[N, G[a] <: Graph[a]] {

  def graph: G[N]

  def rewriter: Transcriber[N, G[N]]

  def tagToNodes: Map[Tag, Seq[N]]

  type _Graph = G[N]
}