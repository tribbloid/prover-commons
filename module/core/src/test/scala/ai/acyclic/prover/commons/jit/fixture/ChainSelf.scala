package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.Hom.:=>
import ai.acyclic.prover.commons.jit.eval.Args.{><:, T0}

object ChainSelf {

  import Circuits.*

  val s0: (Int ><: T0) :=> Int = fn0.andThen(fn0)

  val s1: (Int ><: T0) :=> Int = fn0.trace.map(fn0).unbox

  val s2: (Int ><: T0) :=> Int = :=>.id[Int].trace.map(v => fn0(v)).map(v => fn0(v)).unbox

  lazy val pairs = {

    val str = s"""
                 |+ Mapped
                 |!-- ${fn0.explain.nodeText}
                 |!-- ${fn0.explain.nodeText}
                 |""".stripMargin

    val pairs: Seq[
      ((Int ><: T0) :=> Int, String)
    ] = Seq(
      (s0, str),
      (s1, str),
      (
        s2,
        s"""
           |+ Mapped
           |!-+ Mapped
           |: !-- Identity
           |: !-- Blackbox(s2 <at ChainSelf.scala:14>)
           |!-- Blackbox(s2 <at ChainSelf.scala:14>)
           |""".stripMargin
      )
    )

    pairs
  }

}
