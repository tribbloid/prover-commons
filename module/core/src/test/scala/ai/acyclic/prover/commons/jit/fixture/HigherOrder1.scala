package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.hom.Hom.:=>
import ai.acyclic.prover.commons.jit.tracing._

object HigherOrder1 {

  import Circuits.*

  val s1: (Int :=> Int) :=> (Unit :=> Seq[Int]) = :=>.at { circuit =>
    val result = for (const <- circuit.trace.higherOrder) yield {

      (1 to 10).map(const)
    }

    result
  }

  val s2: Unit :=> Seq[Int] = {
    s1
      .apply(fn0)
  }

  val pairs =
    Seq(
      s1 -> "- Blackbox(s1 <at HigherOrder1.scala:9>)",
      s2 ->
        s"""
          |+ Mapped
          |!-+ Static
          |: !-- ${fn0.explain.nodeText}
          |!-- Blackbox(result <at HigherOrder1.scala:10>)
          |""".stripMargin
    )
}
