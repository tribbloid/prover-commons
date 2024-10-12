package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom.:=>

object ChainOther {

  import Circuits._

  val s1: Int :=> String = fn0.andThen[String] { v =>
    s"${v}b"
  }

  val s2 = for (v <- fn0.trace) yield {
    s"${v}b"
  }

  lazy val pairs = {

    val pairs: Seq[(Int :=> String, String)] = {
      Seq(
        (
          s1,
          s"""
             |+ Compose
             |!-- ${fn0.explain.nodeText}
             |!-- ${fn0.explain.nodeText}
             |""".stripMargin
        ),
        (
          s2,
          s"""
             |+ Compose
             |!-- ${fn0.explain.nodeText}
             |!-- ${fn0.explain.nodeText}
             |""".stripMargin
        )
      )
    }

    pairs
  }
}
