package ai.acyclic.prover.commons.testlib

import ai.acyclic.prover.commons.benchmark.BenchmarkProfile

trait Benchmark extends BaseSpec {

  lazy val profile: BenchmarkProfile = BenchmarkProfile.canonical()
}
