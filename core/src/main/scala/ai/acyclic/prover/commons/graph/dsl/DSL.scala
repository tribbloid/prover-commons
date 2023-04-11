package ai.acyclic.prover.commons.graph.dsl

import ai.acyclic.prover.commons.graph.local.Graph

trait DSL[N, G[a] <: Graph[a]] {

  def graph: G[N]

  type _Graph = G[N]
}
