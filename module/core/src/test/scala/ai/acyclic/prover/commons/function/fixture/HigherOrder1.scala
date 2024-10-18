package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom.:=>

object HigherOrder1 {

  import Circuits._

  val s1: (Int :=> Int) :=> (Unit :=> Seq[Int]) = :=> { circuit =>
    val result = for (const <- circuit.trace.higher) yield {

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
          |!-+ Eager
          |: !-- ${fn0.explain.nodeText}
          |!-- Blackbox(result <at HigherOrder1.scala:10>)
          |""".stripMargin
    )
}
