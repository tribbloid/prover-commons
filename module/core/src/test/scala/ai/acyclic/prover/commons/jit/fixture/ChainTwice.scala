package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.hom.Hom.:=>
import ai.acyclic.prover.commons.jit.tracingV1.Tracing

object ChainTwice {

  import Circuits.*

  val s1: Tracing[Int, String] =
    for (
      x <- fn0.trace;
      y = 3;
      z = y + x
    ) yield {

      s"${x + y + z}b"
    }

  val s1_desugared: Tracing[Int, String] = fn0.trace
    .map { x =>
      val y = 3
      (x, y)
    }
    .map {
      case (x, y) =>
        val z = y + x
        s"${x + y + z}b"
    }

  lazy val pairs: Seq[(Int :=> String, String)] = {

    Seq(
      (
        s1,
        s""" 
             |+ Mapped
             |!-+ Mapped
             |: !-- ${fn0.explain.nodeText}
             |: !-- Blackbox(s1 <at ChainTwice.scala:12>)
             |!-- Blackbox(s1 <at ChainTwice.scala:12>)
             |""".stripMargin
      ),
      (
        s1_desugared,
        s"""
           |+ Mapped
           |!-+ Mapped
           |: !-- ${fn0.explain.nodeText}
           |: !-- Blackbox(s1 <at ChainTwice.scala:12>)
           |!-- Blackbox(s1 <at ChainTwice.scala:12>)
           |""".stripMargin
      )
    )
  }
}
