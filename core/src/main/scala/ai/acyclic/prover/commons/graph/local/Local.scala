package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.GraphSystem

object Local extends GraphSystem {

  override type Dataset[T] = Seq[T]
  def parallelize[T](seq: Seq[T]): Seq[T] = seq
}
