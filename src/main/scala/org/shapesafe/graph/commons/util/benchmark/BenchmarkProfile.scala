package org.shapesafe.graph.commons.util.benchmark

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

case class BenchmarkProfile(
    stages: List[(Tags, BenchmarkLike)]
) extends BenchmarkLike {

  override def run[T](fn: => T): BenchmarkResults[T] = {

    val results = stages.flatMap {
      case (tags, stage) =>
        val result = stage.run(fn)

        val withTags = result.elements.map { v =>
          v.copy(tags = tags)
        }

        withTags
    }

    results
  }
}

object BenchmarkProfile {

  def canonical(
      warmUp: BenchmarkStage = 2 seconds,
      body: BenchmarkStage = 5 seconds
  ): BenchmarkProfile = BenchmarkProfile(
    List(
      Set(BenchmarkTag.WarmUp) -> warmUp,
      Set(BenchmarkTag.Body) -> body
    )
  )
}
