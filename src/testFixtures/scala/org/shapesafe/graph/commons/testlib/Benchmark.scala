package org.shapesafe.graph.commons.testlib

import org.shapesafe.graph.commons.util.benchmark.BenchmarkProfile

trait Benchmark extends BaseSpec {

  lazy val profile: BenchmarkProfile = BenchmarkProfile.canonical()
}
