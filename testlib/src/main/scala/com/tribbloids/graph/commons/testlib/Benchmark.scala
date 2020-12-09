package com.tribbloids.graph.commons.testlib

import com.tribbloids.graph.commons.util.benchmark.BenchmarkProfile

trait Benchmark extends BaseSpec {

  lazy val profile: BenchmarkProfile = BenchmarkProfile.canonical()
}
