package graph.commons.util

import graph.commons.util.benchmark.BenchmarkTag

package object benchmark {

  type Tags = Set[_ <: BenchmarkTag]
}
