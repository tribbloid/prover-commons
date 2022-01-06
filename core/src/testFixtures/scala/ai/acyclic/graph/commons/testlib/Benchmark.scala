package ai.acyclic.graph.commons.testlib

import ai.acyclic.graph.commons.benchmark.BenchmarkProfile

trait Benchmark extends BaseSpec {

  lazy val profile: BenchmarkProfile = BenchmarkProfile.canonical()
}
