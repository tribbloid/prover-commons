package ai.acyclic.graph.commons.benchmark

trait BenchmarkTag extends Product {}

object BenchmarkTag {

  case object Body extends BenchmarkTag

  case object WarmUp extends BenchmarkTag
}
