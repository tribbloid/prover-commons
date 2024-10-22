package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom.Poly

object Polys {

  object _poly extends Poly {

    implicit lazy val int: Int Target Int = {

      at[Int](v => v + 1)
    }

    implicit lazy val str: String Target String = {
      at[String].to[String](v => v + "1")
    }
  }
}
