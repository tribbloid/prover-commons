package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom

object PointwiseAndChain {

  import Circuits._

  private val pointwise: Hom.Circuit.Tracing[(Int, Long), (Seq[Long], Seq[Double])] = fn1.trace >< fn2.trace

  val s1 = pointwise.map {
    case (o1, o2) =>
      o1 -> o2
  }

  val s2 =
    for (case (o1, o2) <- pointwise)
      yield {
        o1 -> o2
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
             |
             |+ Mapped
             |!-+ WithFilter
             |: !-+ Pointwise
             |: : !-- ${fn1.explain.nodeText}
             |: : !-- ${fn2.explain.nodeText}
             |: !-- Blackbox(s2 <at PointwiseAndChain.scala:17>)
             |!-- Blackbox(s2 <at PointwiseAndChain.scala:17>)
             |""".stripMargin
      )
    )
  }
}
