package graph.commons.util.benchmark

trait BenchmarkLike {

  def run[T](fn: => T): BenchmarkResults[T] = Nil
}
