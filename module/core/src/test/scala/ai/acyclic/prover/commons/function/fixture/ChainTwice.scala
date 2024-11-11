package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom.:=>

object ChainTwice {

  import Circuits.*

  val s1 =
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
             |: !-- Blackbox(s1 <at ChainTwice.scala:11>)
             |!-- Blackbox(s1 <at ChainTwice.scala:11>)
             |""".stripMargin
      )
    )
  }
}
