package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.hom.Hom.:=>
import ai.acyclic.prover.commons.jit.tracingV1.Tracing

object ChainTwice {

  import Circuits.*

  val s1: Tracing[Int, String] =
    for (
      v <- fn0.trace;
      v1 = 3;
      v2 = v1 + v
    ) yield {
      s"${v + v1 + v2}b"
    }

  lazy val pairs: Seq[(Int :=> String, String)] = {

    Seq(
      (
        s1,
        s""" 
             |
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
