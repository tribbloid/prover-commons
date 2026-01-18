package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.hom.Hom.:=>
import ai.acyclic.prover.commons.jit.tracing.*

object ChainOther {

  import Circuits.*

  val s1: Int :=> String = fn0.andThen[String] { v =>
    s"${v}b"
  }

  val s2: Int :=> String = for (v <- fn0.trace) yield {
    s"${v}b"
  }

  lazy val pairs: Seq[(Int :=> String, String)] = {

    val pairs: Seq[(Int :=> String, String)] = {
      Seq(
        (
          s1,
          s"""
             |+ Mapped
             |!-- ${fn0.explain.nodeText}
             |!-- Blackbox(s1 <at ChainOther.scala:10>)
             |""".stripMargin
        ),
        (
          s2,
          s""" 
             |+ Mapped
             |!-- ${fn0.explain.nodeText}
             |!-- Blackbox(s2 <at ChainOther.scala:14>)
             |""".stripMargin
        )
//        (s3, ""),
//        (s4, "")
      )
    }

    pairs
  }
}
