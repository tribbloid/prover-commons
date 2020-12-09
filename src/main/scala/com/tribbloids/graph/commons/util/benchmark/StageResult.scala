package com.tribbloids.graph.commons.util.benchmark

case class StageResult[T](
    i: Int,
    v: T,
    startNano: Long,
    finishNano: Long,
    tags: Tags = Set.empty
) {

  lazy val durationNano: Long = finishNano - startNano

  lazy val _tags: Set[BenchmarkTag] = tags.map { v: BenchmarkTag =>
    v
  }
}
