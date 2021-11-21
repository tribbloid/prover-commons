package org.shapesafe.graph.commons.util.benchmark

import org.shapesafe.graph.commons.util.debug.Debug.CallStackRef

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

case class BenchmarkResults[T](
    elements: List[StageResult[T]]
) {

  lazy val valid: List[StageResult[T]] = elements.filterNot { v =>
    v._tags.contains(BenchmarkTag.WarmUp)
  }

  lazy val total: Long = valid.map { v =>
    v.durationNano
  }.sum

  lazy val avg: Double = total.toDouble / valid.size

  def log(): Unit = {

    val ref: CallStackRef = CallStackRef(exclude = Seq(this.getClass))

    val info =
      s"${ref.className} - avg: ${Duration.fromNanos(avg).toUnit(TimeUnit.MILLISECONDS)}ms"

//    LoggerFactory.getLogger(this.getClass).info(info)

    println(info)
  }
}

object BenchmarkResults {

  implicit def fromResults[T](v: List[StageResult[T]]): BenchmarkResults[T] = BenchmarkResults(v)
}
