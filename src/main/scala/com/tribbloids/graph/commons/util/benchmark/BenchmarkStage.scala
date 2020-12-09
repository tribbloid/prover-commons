package com.tribbloids.graph.commons.util.benchmark

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

trait BenchmarkStage extends BenchmarkLike {

  final override def run[T](fn: => T): BenchmarkResults[T] = {

    val result = iterate { i =>
      val start = System.nanoTime()

      val v = fn

      val finish = System.nanoTime()

      StageResult(i, v, start, finish)
    }

    result.toList
  }

  def iterate[T](fn: Int => StageResult[T]): Seq[StageResult[T]]
}

object BenchmarkStage {

  case class Iterations(n: Int) extends BenchmarkStage {

    def iterate[T](fn: Int => StageResult[T]): Seq[StageResult[T]] = {

      val result = for (i <- 0 until n) yield {

        val rr = fn(i)

        rr
      }

      result
    }
  }
  implicit def magnet(n: Int): Iterations = Iterations(n)

  case class Lapse(d: Duration) extends BenchmarkStage {

    override def iterate[T](fn: Int => StageResult[T]): Seq[StageResult[T]] = {

      val result = ArrayBuffer.empty[StageResult[T]]

      val starTime = System.nanoTime()
      val dd = d.toNanos
      var i = 0

      while (System.nanoTime() - starTime < dd) {

        result += fn(i)
        i += 1
      }

      result.toSeq
    }
  }
  implicit def magnet(d: Duration): Lapse = Lapse(d)
}
