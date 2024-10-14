package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom.:=>

object ChainSelf {

  import Circuits._

  val s0: Int :=> Int = fn0.andThen(fn0)

  val s1 = fn0.trace.map(fn0)

  val s2 = :=>.id[Int].trace.map(fn0).map(fn0)

  lazy val pairs = {

    val str = s"""
                 |+ Compose
                 |!-- ${fn0.explain.nodeText}
                 |!-- ${fn0.explain.nodeText}
                 |""".stripMargin

    val pairs: Seq[
      (Int :=> Int, String)
    ] = Seq(
      (s0, str),
      (s1, str),
      (s2, str)
    )

    pairs
  }

}
