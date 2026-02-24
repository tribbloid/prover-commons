package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.cps.Continuation
import ai.acyclic.prover.commons.jit.eval.Args.{><:, T0}

object PointwiseZipLike {

  import Circuits.*

  private val pointwise = fn1.trace <*> fn2.trace

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
      pointwise ->
        s"""
           |+ Mapped
           |!-+ Pointwise
           |: !-- ${fn1.explain.nodeText}
           |: !-- ${fn2.explain.nodeText}
           |!-- Blackbox(s1 <at PointwiseAndChain.scala:12>)
           |""".stripMargin,
      s1 ->
        s"""
            |+ Mapped
            |!-+ Pointwise
            |: !-- ${fn1.explain.nodeText}
            |: !-- ${fn2.explain.nodeText}
            |!-- Blackbox(s1 <at PointwiseAndChain.scala:12>)
            |""".stripMargin,
      s2 ->
        s"""
             |+ Mapped
             |!-+ Mapped
             |: !-+ Pointwise
             |: : !-- ${fn1.explain.nodeText}
             |: : !-- ${fn2.explain.nodeText}
             |: !-- Blackbox(s2 <at PointwiseAndChain.scala:18>)
             |!-- Blackbox(s2 <at PointwiseAndChain.scala:18>)
             |""".stripMargin,
      s3 ->
        s"""
             |+ Mapped
             |!-+ Mapped
             |: !-+ Pointwise
             |: : !-- Blackbox(fn1 <at Circuits.scala:12>)
             |: : !-- Blackbox(fn2 <at Circuits.scala:16>)
             |: !-- Blackbox(s3 <at PointwiseAndChain.scala:25>)
             |!-- Blackbox(s3 <at PointwiseAndChain.scala:25>)
             |""".stripMargin
    )
  }
}
