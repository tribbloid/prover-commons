package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom.Poly

object Polys {

  object _poly extends Poly {

    implicit lazy val int: Int =>> Int = {

      at[Int](v => v + 1)
    }

    implicit lazy val str: String =>> String = {
      at[String].to[String](v => v + "1")
    }
  }
}
