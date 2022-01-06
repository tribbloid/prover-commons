package ai.acyclic.graph.commons.benchmark

trait BenchmarkLike {

  def run[T](fn: => T): BenchmarkResults[T] = Nil
}
