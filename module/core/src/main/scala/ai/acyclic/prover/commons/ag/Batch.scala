package ai.acyclic.prover.commons.ag

trait Batch[+V] {

  def collect: Seq[V]
}
