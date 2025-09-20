package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom

object PointwiseAndChain {

  import Circuits.*

  private val pointwise: Hom.Fn.Tracing[(Int, Long), (Seq[Long], Seq[Double])] = fn1.trace >< fn2.trace

  val s1 = pointwise.map {
    case (o1, o2) =>
      o1.zip(o2).map(v => v._1 + v._2)
  }

  val s2 =
    for (case (o1, o2) <- pointwise)
      yield {
        o1.zip(o2).map(v => v._1 + v._2)
      }

  val s3 =
    for (
      tt <- pointwise;
      o1 = tt._1;
      o2 = tt._2
    ) yield {

      o1.zip(o2).map(v => v._1 + v._2)
    }

  lazy val pairs = {

    Seq(
      (
        s1,
        s"""
           |+ Mapped
           |!-+ Pointwise
           |: !-- ${fn1.explain.nodeText}
           |: !-- ${fn2.explain.nodeText}
           |!-- Blackbox(s1 <at PointwiseAndChain.scala:11>)
           |""".stripMargin
      ),
      (
        s2,
        s"""
             |+ Mapped
             |!-+ Filtered
             |: !-+ Pointwise
             |: : !-- ${fn1.explain.nodeText}
             |: : !-- ${fn2.explain.nodeText}
             |: !-- Blackbox(s2 <at PointwiseAndChain.scala:17>)
             |!-- Blackbox(s2 <at PointwiseAndChain.scala:17>)
             |""".stripMargin
      ),
      (
        s3,
        s"""
             |+ Mapped
             |!-+ Mapped
             |: !-+ Pointwise
             |: : !-- Blackbox(fn1 <at Circuits.scala:11>)
             |: : !-- Blackbox(fn2 <at Circuits.scala:15>)
             |: !-- Blackbox(s3 <at PointwiseAndChain.scala:24>)
             |!-- Blackbox(s3 <at PointwiseAndChain.scala:24>)
             |""".stripMargin
      )
    )
  }
}
