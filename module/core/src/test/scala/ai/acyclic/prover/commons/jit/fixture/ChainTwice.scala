package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.hom.Hom.:=>
import ai.acyclic.prover.commons.jit.tracing.TracingFn
import ai.acyclic.prover.commons.debug.SrcDefinition

object ChainTwice {

  import Circuits.*

  val s1: TracingFn.Static[Int, String] =
    for (
      x <- fn0.trace;
      y = 3;
      z = y + x
    ) yield {

      s"${x + y + z}b"
    }

  val s1_desugared: TracingFn.Static[Int, String] = fn0.trace
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
             |: !-- Blackbox(s1 <at ChainTwice.scala:13>)
             |!-- Blackbox(s1 <at ChainTwice.scala:13>)
             |""".stripMargin
      ),
      (
        s1_desugared,
        s"""
           |+ Mapped
           |!-+ Mapped
           |: !-- ${fn0.explain.nodeText}
           |: !-- Blackbox(s1_desugared <at ChainTwice.scala:22>)
           |!-- Blackbox(s1_desugared <at ChainTwice.scala:26>)
           |""".stripMargin
      )
    )
  }
}
