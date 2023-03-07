package ai.acyclic.prover.commons.graph.local

trait Builder[N, G <: Graph[N]] {

  def build(vs: Seq[N]): G
}
