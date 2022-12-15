package ai.acyclic.prover.commons.benchmark

trait BenchmarkLike {

  def run[T](fn: => T): BenchmarkResults[T] = Nil
}
