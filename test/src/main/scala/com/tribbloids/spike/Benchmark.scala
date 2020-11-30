package com.tribbloids.spike

import graph.commons.util.benchmark.BenchmarkProfile

trait Benchmark extends BaseSpec {

  lazy val profile: BenchmarkProfile = BenchmarkProfile.canonical()
}
