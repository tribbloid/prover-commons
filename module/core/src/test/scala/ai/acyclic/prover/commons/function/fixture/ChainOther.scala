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
             |+ Mapped
             |!-- ${fn0.explain.nodeText}
             |!-- Blackbox(s1 <at ChainOther.scala:9>)
             |""".stripMargin
        ),
        (
          s2,
          s""" 
             |+ Mapped
             |!-- ${fn0.explain.nodeText}
             |!-- Blackbox(s2 <at ChainOther.scala:13>)
             |""".stripMargin
        )
      )
    }

    pairs
  }
}
