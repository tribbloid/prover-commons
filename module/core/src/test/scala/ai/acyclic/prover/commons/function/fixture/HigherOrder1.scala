package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom.:=>

object HigherOrder1 {

  import Circuits._

  val raw: (Int :=> Int) :=> (Unit :=> Seq[Int]) = :=> { circuit =>
    val result = for (const <- circuit.trace.higher) yield {

      (1 to 10).map(const)
    }

    result
  }

  val s1: Unit :=> Seq[Int] = {
    raw
      .apply(fn0)
  }

  val pairs: Seq[(Unit :=> Seq[Int], String)] =
    Seq(
      s1 -> "s1"
//      s2 -> "s2"
    )
}
